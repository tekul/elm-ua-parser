module KAT exposing (tests)

import Expect
import Test exposing (..)
import Tuple exposing (first, second)
import UA exposing (Browser(..), OS(..), UA(..), parse)


chromeKats =
    [ -- Linux, Chrome 74
      ( "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.131 Safari/537.36", UA (Chrome 74) Linux )
    , -- Windoze 10, Chrome 60
      ( "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36", UA (Chrome 60) Windows )
    , -- Android, Chrome 18
      ( "Mozilla/5.0 (Linux; Android 4.0.4; Galaxy Nexus Build/IMM76B) AppleWebKit/535.19 (KHTML, like Gecko) Chrome/18.0.1040.134 Mobile Safari/535.19", UA (Chrome 18) Android )
    , -- iOS, Chrome 56
      ( "Mozilla/5.0 (iPhone; CPU iPhone OS 10_3 like Mac OS X) AppleWebKit/602.1.50 (KHTML, like Gecko) CriOS/56.0.2924.75 Mobile/14E5239e Safari/602.1", UA (Chrome 56) IOS )
    ]


firefoxKats =
    [ -- Linux, Firefox 66
      ( "Mozilla/5.0 (X11; Linux x86_64; rv:66.0) Gecko/20100101 Firefox/66.0", UA (Firefox 66) Linux )
    , -- Windows, FF 47
      ( "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0", UA (Firefox 47) Windows )
    , ( "Mozilla/5.0 (Android 4.4; Tablet; rv:41.0) Gecko/41.0 Firefox/41.0", UA (Firefox 41) Android )
    ]


ie11Kats =
    [ ( "Mozilla/5.0 (Windows NT 6.1; Trident/7.0; rv:11.0) like Gecko", UA IE11 Windows )
    , ( "Mozilla/5.0 (Windows NT 10.0; WOW64; Trident/7.0; Touch; rv:11.0) like Gecko", UA IE11 Windows )
    ]


ieKats =
    [ -- Windows 8, IE10
      ( "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2)", UA (IE 10) Windows )
    , -- Windows 7, IE10
      ( "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)", UA (IE 10) Windows )
    , -- Windows 7, IE 9
      ( "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)", UA (IE 9) Windows )
    ]


safariKats =
    [ -- OSX, Safari 5.1
      ( "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/534.59.10 (KHTML, like Gecko) Version/5.1.9 Safari/534.59.10", UA Safari OSX )

    -- IOS, Safari 10
    , ( "Mozilla/5.0 (iPhone; CPU iPhone OS 10_3 like Mac OS X) AppleWebKit/603.1.23 (KHTML, like Gecko) Version/10.0 Mobile/14E5239e Safari/602.1", UA Safari IOS )
    ]


tests : Test
tests =
    let
        katTest k =
            test (first k) (\_ -> Expect.equal (second k) (parse (first k)))
    in
    describe "KAT Tests"
        [ describe "Chrome"
            (List.map katTest chromeKats)
        , describe "Firefox"
            (List.map katTest firefoxKats)
        , describe "Safari"
            (List.map katTest safariKats)
        , describe "IE11"
            (List.map katTest ie11Kats)
        , describe "IE"
            (List.map katTest ieKats)
        ]
