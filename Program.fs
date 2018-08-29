// Learn more about F# at http://fsharp.org

open System
open System.Text.RegularExpressions
open System.IO

open canopy.runner.classic
open canopy.configuration
open canopy.classic
open canopy
open canopy

open FSharp.Data
open System.Net

let getMonthYears () : (OpenQA.Selenium.IWebElement * OpenQA.Selenium.IWebElement) list = 
    let rec getMonthYearHelper num res = 
        let monthXPath = sprintf @"//*[@id=""app""]/div[4]/div[1]/ul/li[%d]/div/div/div/div/span[%d]" num 1
        let yearXPath = sprintf @"//*[@id=""app""]/div[4]/div[1]/ul/li[%d]/div/div/div/div/span[%d]" num 2

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
    

let getUrls (monthYears : (OpenQA.Selenium.IWebElement * OpenQA.Selenium.IWebElement) list) =
    let helper (month, year) = 
        click month
        sleep 10
        let urlParts = elements "//li/div"
                        |> List.map (fun div -> div, urlRegex.Match(div.GetAttribute "style"))
                        |> List.filter (fun (div, m) -> m.Success)
                        |> List.map (fun (div, m) ->
                                        let day = 
                                            div 
                                            |> someElementWithin ".name"
                                            |> Option.map (elementWithin "span") 
                                            |> Option.bind convertToDay
                                        let url = sprintf "https://www.tadpoles.com%s" (m.Groups.[1].Value.Replace("thumbnail=true", "").Replace("&thumbnail=true", ""))
                                        url, year, month, day
                                        )
        printfn "Found %d urls" (List.length urlParts)
        urlParts
                  
                               
    List.collect helper monthYears

let customizeRequest (cookies : Collections.ObjectModel.ReadOnlyCollection<OpenQA.Selenium.Cookie>) (req : Net.HttpWebRequest) : Net.HttpWebRequest =
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

let processUrl (dir : string) (cookies : Collections.ObjectModel.ReadOnlyCollection<OpenQA.Selenium.Cookie>) ((url, year, month, day) : string * OpenQA.Selenium.IWebElement * OpenQA.Selenium.IWebElement * string option) = 
    printfn "Url: %s" url
    match url.Split("key=") with
    | [|_;key|] -> 
        let httpResponse = Http.Request(url, customizeHttpRequest = (customizeRequest cookies))
        let filename = 
            match httpResponse.Headers.["Content-Type"] with 
            | "video/mp4" -> Some <| Path.Combine(dir, year.Text, month.Text, sprintf "%s_%s_%s_%s.mp4" year.Text month.Text (Option.defaultValue "" day) key)
            | "image/jpeg" -> Some <| Path.Combine(dir, year.Text, month.Text, sprintf "%s_%s_%s_%s.jpg" year.Text month.Text (Option.defaultValue "" day) key)
            | _ -> None
        match filename with
        | None -> ()
        | Some fname ->        
            if File.Exists fname then
                printfn "File exists: %s" fname
            else 
                printfn "Saving %s" fname
                Directory.CreateDirectory (Path.GetDirectoryName(fname)) |> ignore
                match httpResponse.Body with
                | Binary bytes ->
                    File.WriteAllBytes(fname, bytes)
                | _ -> printfn "Failed"        
    | _ -> ()

let app user pass dir = 
    start chrome
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
    getMonthYears()
    |> List.map (fun my -> printfn "MonthYear: %A" my
                           my)
    |> getUrls
    |> List.iter (processUrl dir cookies)

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