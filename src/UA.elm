module UA exposing (Browser(..), OS(..), UA(..), parse)

import Parser exposing (..)
import Tuple exposing (pair)


type OS
    = Linux
    | OSX
    | Android
    | IOS
    | Windows
    | UnknownOS


type Browser
    = Chrome Int
    | Firefox Int
    | IE11
    | IE Int
    | Edge
    | Safari
    | Other


type UA
    = UA Browser OS


parse : String -> UA
parse uaString =
    case Parser.run mozilla5Parser uaString of
        Err es ->
            logErrors uaString es (UA Other UnknownOS)

        Ok ua ->
            ua


logErrors : String -> List Parser.DeadEnd -> a -> a
logErrors target deadEnds a =
    Debug.log (Debug.toString <| List.map (toErrorMsg target) deadEnds) a


toErrorMsg : String -> DeadEnd -> String
toErrorMsg uaString deadEnd =
    case deadEnd.problem of
        Expecting s ->
            "Expected " ++ s ++ " at '" ++ String.dropLeft deadEnd.col uaString ++ "'"

        e ->
            Debug.toString e


type MozillaVersion
    = Mozilla5
    | MozillaOutdated


mozilla5Parser : Parser UA
mozilla5Parser =
    succeed (\sysinfo browser -> parseSysInfo sysinfo browser)
        |. token "Mozilla/5.0"
        |. spaces
        |. symbol "("
        |= getChompedString (chompUntil ")")
        |. symbol ")"
        |. spaces
        |= oneOf
            [ webKitParser
            , geckoParser
            , ie11Parser
            , succeed Other
            ]


parseSysInfo : String -> Browser -> UA
parseSysInfo sysinfo browser =
    let
        parser =
            case browser of
                Other ->
                    uaFromSysinfoParser

                _ ->
                    standardSysinfoParser browser
    in
    case Parser.run parser sysinfo of
        Err es ->
            logErrors sysinfo es (UA Other UnknownOS)

        Ok ua ->
            ua


standardSysinfoParser : Browser -> Parser UA
standardSysinfoParser browser =
    oneOf
        [ map (\_ -> UA browser Windows) (token "Windows NT")
        , map (\_ -> UA browser Linux) (token "X11; Linux")
        , map (\_ -> UA browser Android)
            (oneOf [ token "Linux; Android", token "Android" ])
        , map (\_ -> UA browser OSX) (token "Macintosh")
        , map (\_ -> UA browser IOS) (token "iPhone")
        ]


uaFromSysinfoParser : Parser UA
uaFromSysinfoParser =
    succeed (\v -> UA (IE v) Windows)
        |. token "compatible; MSIE "
        |= majorVersion
        |. chompUntil " "
        |. token " Windows NT"


webKitParser : Parser Browser
webKitParser =
    let
        chrome =
            succeed Chrome
                |. symbol "C"
                |. oneOf [ token "riOS", token "hrome" ]
                |. symbol "/"
                |= majorVersion

        safari =
            token "Version/"
                |. chompUntil " "
                |. spaces
                |. chompUntil "Safari"
    in
    succeed identity
        |. token "AppleWebKit/"
        |. chompUntil " "
        |. spaces
        |. token "(KHTML, like Gecko)"
        |. spaces
        |= oneOf
            [ chrome
            , map (\_ -> Safari) safari
            ]


geckoParser : Parser Browser
geckoParser =
    succeed Firefox
        |. token "Gecko/"
        |. chompUntil " "
        |. spaces
        |. token "Firefox/"
        |= majorVersion


ie11Parser : Parser Browser
ie11Parser =
    succeed IE11
        |. token "like Gecko"


majorVersion : Parser Int
majorVersion =
    getChompedString (chompUntil ".")
        |> andThen
            (\v ->
                case String.toInt v of
                    Just version ->
                        succeed version

                    Nothing ->
                        problem ("Expected major version but got: " ++ v)
            )
