// Learn more about F# at http://fsharp.org

open System
open System.Text.RegularExpressions
open System.IO

open canopy.classic
open canopy

open FSharp.Data
open System.Net
open OpenQA.Selenium
open System.Collections.Generic
open Argu

type LoginType = | Google | Tadpoles

type Url = {
    url : string
    key : string
    year : int
    month : string
    day : int
    keyHash : string
}

type Args =
| [<Mandatory>]Type of LoginType
| [<Mandatory>]Username of string
| [<Mandatory>]Password of string
| Dir of string
with
    interface IArgParserTemplate with
        member s.Usage = 
            match s with
            | Type _ -> "specify a type"
            | Username _ -> "specify a username"
            | Password _ -> "specify a password"
            | Dir _ -> "specify a directory to save the images"

let MD5Hash (input : string) =
      use md5 = System.Security.Cryptography.MD5.Create()
      input
      |> System.Text.Encoding.ASCII.GetBytes
      |> md5.ComputeHash
      |> Seq.map (fun c -> c.ToString("X2"))
      |> Seq.reduce (+)

let getMonthYears () : (OpenQA.Selenium.IWebElement * OpenQA.Selenium.IWebElement) list = 
    let rec getMonthYearHelper num res = 
        let xPath n = sprintf @"//*[@id=""app""]/div[4]/div[1]/ul/li[%d]/div/div/div/div/span[%d]" num n
        let monthXPath = xPath 1
        let yearXPath = xPath 2

        match someElement monthXPath with
        | None -> res
        | Some el ->
            let yearEl = element yearXPath
            getMonthYearHelper (num + 1) <| List.append [(el, yearEl)] res
    getMonthYearHelper 1 []

let urlRegex = Regex(@"\(""([^""]+)")

let convertToDay (element : OpenQA.Selenium.IWebElement) = 
    match element.Text.Split("/") with
    | [|_;day|] ->
        Some day
    | _ -> None
    

let getDayFromElement div =
    div 
    |> someElementWithin ".name"
    |> Option.map (elementWithin "span") 
    |> Option.bind convertToDay

let getUrls (monthYears : (OpenQA.Selenium.IWebElement * OpenQA.Selenium.IWebElement) list) =
    let helper ((month, year) : (OpenQA.Selenium.IWebElement * OpenQA.Selenium.IWebElement)) = 
        click month
        sleep 10
        let urlParts = elements "//li/div"
                        |> List.map (fun div -> div, urlRegex.Match(div.GetAttribute "style"))
                        |> List.filter (fun (div, m) -> m.Success && Option.isSome (getDayFromElement div))                        
                        |> List.map (fun (div, m) ->
                                        let day = getDayFromElement div |> Option.get
                                        let url = sprintf "https://www.tadpoles.com%s" (m.Groups.[1].Value.Replace("thumbnail=true", "").Replace("&thumbnail=true", ""))
                                        url, year, month, day
                                        )
        printfn "Found %d urls for %s %s" (List.length urlParts) (month.Text) (year.Text)
        urlParts                  
                               
    List.collect helper monthYears

let customizeRequest (cookies : ICollection<Cookie>) (req : Net.HttpWebRequest) : Net.HttpWebRequest =
    let cookieContainer = Net.CookieContainer()
    cookies
    |> Seq.iter (fun cookie -> 
                    let newCookie = Net.Cookie(cookie.Name, cookie.Value, cookie.Path)
                    newCookie.Secure <- cookie.Secure
                    newCookie.Domain <- cookie.Domain
                    newCookie.HttpOnly <- cookie.IsHttpOnly

                    cookieContainer.Add(newCookie)
                )
    req.CookieContainer <- cookieContainer
    req

let convertToKey ((url, year, month, day) : string * IWebElement * IWebElement * string)  = 
    match url.Split ("key=") with
    | [|_;key|] ->
        Some { url = url
               key = key
               year = int year.Text
               month = month.Text
               day = int day 
               keyHash = MD5Hash key
             }
    | _ -> None

let fileExists (filenameWithoutExtension : string) (contents : string seq) : bool =
    contents
    |> Seq.exists (fun s -> s.StartsWith(filenameWithoutExtension, StringComparison.InvariantCultureIgnoreCase))

let processUrl (dir : string) (cookies : ICollection<Cookie>) (contents : string seq) (url : Url) = 
    printfn "Url: %s" url.url
    let filenameWithoutExtension = Path.Combine(dir, string url.year, url.month, sprintf "%d_%s_%d_%s" url.year url.month url.day url.keyHash)
    if fileExists filenameWithoutExtension contents then
        printfn "File exists: %s" filenameWithoutExtension
    else        
        let httpResponse = Http.Request(url.url, customizeHttpRequest = (customizeRequest cookies))    
        let filename = 
            match httpResponse.Headers.["Content-Type"] with 
            | "video/mp4" -> Some <| sprintf "%s.mp4" filenameWithoutExtension
            | "image/jpeg" -> Some <| sprintf "%s.jpg" filenameWithoutExtension
            | _ -> None
        match filename with
        | None -> 
            printfn "Invalid filetype in response"
        | Some fname ->        
            printfn "Saving %s" fname
            Directory.CreateDirectory (Path.GetDirectoryName(fname)) |> ignore
            match httpResponse.Body with
            | Binary bytes ->
                File.WriteAllBytes(fname, bytes)
            | _ -> printfn "Failed"        

let app loginType user pass dir = 
    canopy.configuration.compareTimeout <- 20.0
    canopy.configuration.chromeDir <- AppContext.BaseDirectory    
    start types.BrowserStartMode.Chrome
    url "https://www.tadpoles.com/"
    click "#login-button"
    click (first ".tp-block-half")
    match loginType with
    | Google ->
        let origWindow = browser.CurrentWindowHandle
        click ".other-login-button"
        let newWindow = browser.WindowHandles |> Seq.find (fun w -> w <> origWindow )
        browser.SwitchTo().Window(newWindow) |> ignore
        "#identifierId" << user    
        click "#identifierNext"
        "[type=password]" << pass
        click "#passwordNext"
        try
         let newWindow = browser.WindowHandles |> Seq.find (fun w -> w <> origWindow )
         browser.SwitchTo().Window(newWindow) |> ignore
         printf "Please enter 2 factory auth pin: "
         let fact = Console.ReadLine()
         "#totpPin" << fact
         click "#totpNext"
         with _ ->
             ()
        browser.SwitchTo().Window(origWindow) |> ignore
    | Tadpoles ->
        click (first ".other-login-button")
        first "input" << user
        "[type=password]" << pass
        click ".btn-primary"        
    waitForElement ".thumbnails"
    let cookies = browser.Manage().Cookies.AllCookies
    Directory.CreateDirectory dir |> ignore
    let directoryContents = Directory.EnumerateFiles(dir, "*.*", SearchOption.AllDirectories)
    getMonthYears()   
    |> getUrls
    |> List.map convertToKey
    |> List.filter (Option.isSome)
    |> List.map (Option.get)
    |> List.iter (processUrl dir cookies directoryContents)    
    quit ()

let getLineWithoutEcho () : string =
    let rec helper str = 
        let key = Console.ReadKey(true)
        if key.Key = ConsoleKey.Enter then
            str
        else
            helper (str + string key.KeyChar)
    helper ""

let getLoginTypeText loginType = 
    match loginType with
    | Google -> "Google"
    | Tadpoles -> "Tadpoles"

[<EntryPoint>]
let main argv =
    match argv with
    | [||] ->  
        printfn "Please select a type of login:"
        printfn "1: Google"
        printfn "2: Tadpoles"
        let type_ = Console.ReadLine()
        let loginType = match type_ with 
                        | "1"  -> Some Google
                        | "2" -> Some Tadpoles
                        | _ -> None
        match loginType with
        | None -> printfn "Invalid login type. Please try again."
        | Some loginType_ -> 
            printf "Please enter your %s email: " (getLoginTypeText loginType_)
            let user = Console.ReadLine()
            printf "Please enter your %s password: " (getLoginTypeText loginType_)
            let pass = getLineWithoutEcho()
            printfn ""
            printf "Please enter a directory to put the images or ENTER for default: "
            let dir = Console.ReadLine()
            app loginType_ user pass (if String.IsNullOrEmpty(dir) then "img" else dir)
    | _ ->        
        let parser = ArgumentParser.Create<Args>(programName = "tadpoles-scraper.exe")        
        let results = parser.Parse argv
        let loginType = results.GetResult (Type)
        let username = results.GetResult(Username)
        let password = results.GetResult(Password)
        let dir = results.GetResult(Dir, defaultValue = "img")
        app loginType username password dir
    0