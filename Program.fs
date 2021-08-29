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
open System.Text

type LoginType =
    | Google
    | Tadpoles

type Url =
    { url: string
      key: string
      year: int
      month: string
      day: int
      keyHash: string }

type Args =
    | [<Mandatory>] Type of LoginType
    | [<Mandatory>] ProfilePath of string
    | Username of string
    | Password of string
    | Dir of string
    | StartingMonth of string
    | StartingYear of string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | ProfilePath _ -> "specify Firefox default profile path"
            | Type _ -> "specify a type"
            | Username _ -> "specify a username"
            | Password _ -> "specify a password"
            | Dir _ -> "specify a directory to save the images"
            | StartingMonth _ -> "specify a starting month (string)"
            | StartingYear _ -> "specify a starting year"

let MD5Hash (input: string) =
    use md5 =
        System.Security.Cryptography.MD5.Create()

    input
    |> System.Text.Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)

let convertMonth =
    function
    | "feb" -> 2
    | "mar" -> 3
    | "apr" -> 4
    | "may" -> 5
    | "jun" -> 6
    | "jul" -> 7
    | "aug" -> 8
    | "sep" -> 9
    | "oct" -> 10
    | "nov" -> 11
    | "dec" -> 12
    | _ -> 1

let getMonthYears starting =
    let rec getMonthYearHelper num res =
        let xPath n =
            sprintf @"//*[@id=""app""]/div[4]/div[2]/ul/div[%d]/div/div/div/div/span[%d]" num n

        let clickableXPath =
            sprintf @"//*[@id=""app""]/div[4]/div[2]/ul/div[%d]" num

        let monthXPath = xPath 1
        let yearXPath = xPath 2

        match someElement monthXPath with
        | None -> res
        | Some el ->
            match someElement clickableXPath with
            | None -> res
            | Some clickableElement ->
                let yearEl = element yearXPath
                let month = el.Text
                let year = yearEl.Text |> int

                let ``include`` =
                    match starting with
                    | Some (m, y) ->
                        DateTime(y, m, 1)
                        >= DateTime(year, convertMonth month, 1)
                    | None -> true

                getMonthYearHelper (num + 1)
                <| (if ``include`` then
                        List.append [ (clickableElement, month, year) ] res
                    else
                        res)

    getMonthYearHelper 1 []

let urlRegex = Regex(@"\(""([^""]+)")

let convertToDay (element: OpenQA.Selenium.IWebElement) =
    match element.Text.Split("/") with
    | [| _; day |] -> Some day
    | _ -> None


let getDayFromElement div =
    div
    |> someElementWithin ".name"
    |> Option.map (elementWithin "span")
    |> Option.bind convertToDay

let getUrls monthYears =
    let helper ((clickable, month, year): (OpenQA.Selenium.IWebElement * string * int)) =
        click clickable
        sleep 10

        let noElementsEl =
            (element "no events for the month" |> parent)
                .GetAttribute("style")

        if not (noElementsEl.Contains("display: none")) then
            printfn "Found 0 urls for %s %i" month year
            []
        else
            let urlParts =
                unreliableElements "//li/div"
                |> List.map (fun div -> div, urlRegex.Match(div.GetAttribute "style"))
                |> List.filter (fun (div, m) -> m.Success && Option.isSome (getDayFromElement div))
                |> List.map
                    (fun (div, m) ->
                        let day =
                            getDayFromElement div |> Option.get |> int

                        let url =
                            sprintf
                                "https://www.tadpoles.com%s"
                                (m.Groups.[1]
                                    .Value.Replace("thumbnail=true", "")
                                    .Replace("&thumbnail=true", ""))

                        url, year, month, day)

            printfn "Found %d urls for %s %i" (List.length urlParts) month year
            urlParts

    monthYears |> List.rev |> List.collect helper

let customizeRequest (cookies: ICollection<Cookie>) (req: Net.HttpWebRequest) : Net.HttpWebRequest =
    let cookieContainer = Net.CookieContainer()

    cookies
    |> Seq.iter
        (fun cookie ->
            let newCookie =
                Net.Cookie(cookie.Name, cookie.Value, cookie.Path)

            newCookie.Secure <- cookie.Secure
            newCookie.Domain <- cookie.Domain
            newCookie.HttpOnly <- cookie.IsHttpOnly

            cookieContainer.Add(newCookie))

    req.CookieContainer <- cookieContainer
    req

let convertToKey ((url, year, month, day): string * int * string * int) =
    match url.Split("key=") with
    | [| _; key |] ->
        Some
            { url = url
              key = key
              year = year
              month = month
              day = day
              keyHash = MD5Hash key }
    | _ -> None

let fileExists (filenameWithoutExtension: string) (contents: string seq) : bool =
    contents
    |> Seq.exists (fun s -> s.StartsWith(filenameWithoutExtension, StringComparison.InvariantCultureIgnoreCase))

let processUrl (dir: string) (cookies: ICollection<Cookie>) (contents: string seq) (url: Url) =
    printfn "Url: %s" url.url

    let filenameWithoutExtension =
        Path.Combine(dir, string url.year, url.month, sprintf "%d_%s_%d_%s" url.year url.month url.day url.keyHash)

    if fileExists filenameWithoutExtension contents then
        printfn "File exists: %s" filenameWithoutExtension
    else
        let httpResponse =
            Http.Request(url.url, customizeHttpRequest = (customizeRequest cookies))

        let filename =
            match httpResponse.Headers.["Content-Type"] with
            | "video/mp4" -> Some <| sprintf "%s.mp4" filenameWithoutExtension
            | "image/jpeg" -> Some <| sprintf "%s.jpg" filenameWithoutExtension
            | _ -> None

        match filename with
        | None -> printfn "Invalid filetype in response"
        | Some fname ->
            printfn "Saving %s" fname

            Directory.CreateDirectory(Path.GetDirectoryName(fname))
            |> ignore

            match httpResponse.Body with
            | Binary bytes -> File.WriteAllBytes(fname, bytes)
            | _ -> printfn "Failed"


let app loginType profilePath username password dir starting =
    CodePagesEncodingProvider.Instance.GetEncoding(437)
    |> ignore

    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)
    canopy.configuration.compareTimeout <- 20.0
    canopy.configuration.chromeDir <- AppContext.BaseDirectory

    let profile =
        Firefox.FirefoxProfile(
            profilePath

        )

    profile.SetPreference("dom.webdriver.enabled", false)
    profile.SetPreference("useAutomationExtension", false)
    profile.WriteToDisk()

    start (types.BrowserStartMode.FirefoxWithProfileAndTimeSpan(profile, TimeSpan.FromSeconds 120.))
    url "https://www.tadpoles.com/home_or_work"
    click "#login-button"
    click (first ".tp-block-half")

    match loginType with
    | Google -> ()
    | Tadpoles ->
        click (first ".other-login-button")
        first "input" << username
        "[type=password]" << password
        click ".btn-primary"

    waitForElement ".thumbnails"
    let cookies = browser.Manage().Cookies.AllCookies
    Directory.CreateDirectory dir |> ignore

    let directoryContents =
        Directory.EnumerateFiles(dir, "*.*", SearchOption.AllDirectories)

    getMonthYears starting
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

        let loginType =
            match type_ with
            | "1" -> Some Google
            | "2" -> Some Tadpoles
            | _ -> None

        match loginType with
        | None -> printfn "Invalid login type. Please try again."
        | Some Tadpoles ->
            printf "Please enter your Firefox default profile path: "
            let profilePath = Console.ReadLine()
            printf "Please enter your %s email: " (getLoginTypeText Tadpoles)
            let user = Console.ReadLine()
            printf "Please enter your %s password: " (getLoginTypeText Tadpoles)
            let pass = getLineWithoutEcho ()
            printfn ""
            printf "Please enter a directory to put the images or ENTER for default: "
            let dir = Console.ReadLine()

            printfn ""
            printf "Please enter a month,year to start from (or press ENTER for all): "
            let startingMonthYear = Console.ReadLine()

            app
                Tadpoles
                profilePath
                user
                pass
                (if String.IsNullOrEmpty(dir) then
                     "img"
                 else
                     dir)
                (if startingMonthYear <> "" then
                     let my = startingMonthYear.Split(",")
                     Some(convertMonth my.[0], int my.[1])
                 else
                     None)
        | Some Google ->
            printf "Please enter your Firefox default profile path: "
            let profilePath = Console.ReadLine()

            printf "Please enter a directory to put the images or ENTER for default: "
            let dir = Console.ReadLine()

            printfn ""
            printf "Please enter a month,year to start from (or press ENTER for all): "
            let startingMonthYear = Console.ReadLine()

            app
                Google
                profilePath
                ""
                ""
                (if String.IsNullOrEmpty(dir) then
                     "img"
                 else
                     dir)
                (if startingMonthYear <> "" then
                     let my = startingMonthYear.Split(",")
                     Some(convertMonth my.[0], int my.[1])
                 else
                     None)
    | _ ->
        let parser =
            ArgumentParser.Create<Args>(programName = "tadpoles-scraper.exe")

        let results = parser.Parse argv
        let loginType = results.GetResult(Type)
        let username = results.GetResult(Username)
        let password = results.GetResult(Password)
        let profilePath = results.GetResult(ProfilePath)

        let dir =
            results.GetResult(Dir, defaultValue = "img")

        app loginType profilePath username password dir None

    0
