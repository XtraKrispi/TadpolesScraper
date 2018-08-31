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

type Url = {
    url : string
    key : string
    year : int
    month : string
    day : int
    keyHash : string
}

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

let app user pass dir = 
    canopy.configuration.chromeDir <- AppContext.BaseDirectory    
    start types.BrowserStartMode.Chrome
    url "https://www.tadpoles.com/"
    let origWindow = browser.CurrentWindowHandle
    click "#login-button"
    click (first ".tp-block-half")
    click ".other-login-button"
    let newWindow = browser.WindowHandles |> Seq.find (fun w -> w <> origWindow )
    browser.SwitchTo().Window(newWindow) |> ignore
    "#identifierId" << user    
    click "#identifierNext"
    "[type=password]" << pass
    click "#passwordNext"
    browser.SwitchTo().Window(origWindow) |> ignore   
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

let getLineWithoutEcho () : string =
    let rec helper str = 
        let key = Console.ReadKey(true)
        if key.Key = ConsoleKey.Enter then
            str
        else
            helper (str + string key.KeyChar)
    helper ""

[<EntryPoint>]
let main argv =
    match argv with
    | [|user;pass|] ->
        app user pass "img"
    | [|user;pass;dir|] ->
        app user pass dir
    | _ -> 
        printf "Please enter your Google email: "
        let user = Console.ReadLine()
        printf "Please enter your Google password: "
        let pass = getLineWithoutEcho()
        printf "Please enter a directory to put the images or ENTER for default: "
        let dir = Console.ReadLine()
        app user pass (if String.IsNullOrEmpty(dir) then "img" else dir)
    0