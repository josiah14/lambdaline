-- NOTICE: to avoid name clashes, the appended numbers to the color names
-- do not match the standard XTerm names of the colors.
--
-- ORGANIZATION: the colors are listed in ascending order according to
-- their XTerm color number from 0 - 255.  If you edit this file, you must ensure this
-- ordering is preserved before submitting a pull/merge request.
module LambdaLine.XTerm.Colors
( Color
, defaultBlack
, defaultDarkRed
, defaultDarkGreen
, defaultDarkYellow
, defaultDarkBlue
, defaultDarkMagenta
, defaultDarkCyan
, defaultLightGrey
, defaultDarkGrey
, defaultRed
, defaultGreen
, defaultYellow
, defaultBlue
, defaultMagenta
, defaultCyan
, defaultWhite
, grey0
, navyBlue
, darkBlue
, blue
, blue0
, blue1
, darkGreen
, deepSkyBlue
, deepSkyBlue0
, deepSkyBlue1
, dodgerBlue
, dodgerBlue0
, green
, springGreen
, turquoise
, deepSkyBlue2
, deepSkyBlue3
, dodgerBlue1
, green0
, springGreen0
, darkCyan
, lightSeaGreen
, deepSkyBlue4
, deepSkyBlue5
, green1
, springGreen1
, springGreen2
, cyan
, darkTurquoise
, turquoise0
, green2
, springGreen3
, springGreen4
, mediumSpringGreen
, cyan0
, cyan1
, darkRed
, deepPink
, purple
, purple0
, purple1
, blueViolet
, orange
, grey37
, mediumPurple
, slateBlue
, slateBlue0
, royalBlue
, chartreuse
, darkSeaGreen
, paleTurquoise
, steelBlue
, steelBlue0
, cornflowerBlue
, chartreuse0
, darkSeaGreen0
, cadetBlue
, cadetBlue0
, skyBlue
, steelBlue1
, chartreuse1
, paleGreen
, seaGreen
, aquamarine
, mediumTurquoise
, steelBlue2
, chartreuse2
, seaGreen0
, seaGreen1
, seaGreen2
, aquamarine0
, darkSlateGrey
, darkRed0
, deepPink0
, darkMagenta
, darkMagenta0
, darkViolet
, purple2
, orange0
, lightPink
, plum
, mediumPurple0
, mediumPurple1
, slateBlue1
, yellow
, wheat
, grey53
, lightSlateGrey
, mediumPurple2
, lightSlateBlue
, yellow0
, darkOliveGreen
, darkSeaGreen1
, lightSkyBlue
, lightSkyBlue0
, skyBlue0
, chartreuse3
, darkOliveGreen0
, paleGreen0
, darkSeaGreen2
, darkSlateGrey0
, skyBlue1
, chartreuse4
, lightGreen
, lightGreen0
, paleGreen1
, aquamarine1
, darkSlateGrey1
, red
, deepPink1
, mediumVioletRed
, magenta
, darkViolet0
, purple3
, darkOrange
, indianRed
, hotPink
, mediumOrchid
, mediumOrchid0
, mediumPurple3
, darkGoldenrod
, lightSalmon
, rosyBrown
, grey63
, mediumPurple4
, mediumPurple5
, gold
, darkKhaki
, navajoWhite
, grey69
, lightSteelBlue
, lightSteelBlue0
, yellow1
, darkOliveGreen1
, darkSeaGreen3
, darkSeaGreen4
, lightCyan
, lightSkyBlue1
, greenYellow
, darkOliveGreen2
, paleGreen2
, darkSeaGreen5
, darkSeaGreen6
, paleTurquoise0
, red0
, deepPink2
, deepPink3
, magenta0
, magenta1
, magenta2
, darkOrange0
, indianRed0
, hotPink0
, hotPink1
, orchid
, mediumOrchid1
, orange1
, lightSalmon0
, lightPink0
, pink
, plum0
, violet
, gold0
, lightGoldenrod
, tan0
, mistyRose
, thistle
, plum1
, yellow2
, khaki
, lightGoldenrod0
, lightYellow
, grey84
, lightSteelBlue1
, yellow3
, darkOliveGreen3
, darkOliveGreen4
, darkSeaGreen7
, honeydew
, lightCyan0
, red1
, deepPink4
, deepPink5
, deepPink6
, magenta3
, magenta4
, orangeRed
, indianRed1
, indianRed2
, hotPink2
, hotPink3
, mediumOrchid2
, darkOrange1
, salmon
, lightCoral
, paleVioletRed
, orchid0
, orchid1
, orange2
, sandyBrown
, lightSalmon1
, lightPink1
, pink0
, plum2
, gold1
, lightGoldenrod1
, lightGoldenrod2
, navajoWhite0
, mistyRose0
, thistle0
, yellow4
, lightGoldenrod3
, khaki0
, wheat0
, cornsilk
, grey100
, grey3
, grey7
, grey11
, grey15
, grey19
, grey23
, grey27
, grey30
, grey35
, grey39
, grey42
, grey46
, grey50
, grey54
, grey58
, grey62
, grey66
, grey70
, grey74
, grey78
, grey82
, grey85
, grey89
, grey93
) where

type Color = String

defaultBlack :: Color
defaultBlack = "0"

defaultDarkRed :: Color
defaultDarkRed = "1"

defaultDarkGreen :: Color
defaultDarkGreen = "2"

defaultDarkYellow :: Color
defaultDarkYellow = "3"

defaultDarkBlue :: Color
defaultDarkBlue = "4"

defaultDarkMagenta :: Color
defaultDarkMagenta = "5"

defaultDarkCyan :: Color
defaultDarkCyan = "6"

defaultLightGrey :: Color
defaultLightGrey = "7"

defaultDarkGrey :: Color
defaultDarkGrey = "8"

defaultRed :: Color
defaultRed = "9"

defaultGreen :: Color
defaultGreen = "10"

defaultYellow :: Color
defaultYellow = "11"

defaultBlue :: Color
defaultBlue = "12"

defaultMagenta :: Color
defaultMagenta = "13"

defaultCyan :: Color
defaultCyan = "14"

defaultWhite :: Color
defaultWhite = "15"

grey0 :: Color
grey0 = "16"

navyBlue :: Color
navyBlue = "17"

darkBlue :: Color
darkBlue = "18"

blue :: Color
blue = "19"

blue0 :: Color
blue0 = "20"

blue1 :: Color
blue1 = "21"

darkGreen :: Color
darkGreen = "22"

deepSkyBlue :: Color
deepSkyBlue = "23"

deepSkyBlue0 :: Color
deepSkyBlue0 = "24"

deepSkyBlue1 :: Color
deepSkyBlue1 = "25"

dodgerBlue :: Color
dodgerBlue = "26"

dodgerBlue0 :: Color
dodgerBlue0 = "27"

green :: Color
green = "28"

springGreen :: Color
springGreen = "29"

turquoise :: Color
turquoise = "30"

deepSkyBlue2 :: Color
deepSkyBlue2 = "31"

deepSkyBlue3 :: Color
deepSkyBlue3 = "32"

dodgerBlue1 :: Color
dodgerBlue1 = "33"

green0 :: Color
green0 = "34"

springGreen0 :: Color
springGreen0 = "35"

darkCyan :: Color
darkCyan = "36"

lightSeaGreen :: Color
lightSeaGreen = "37"

deepSkyBlue4 :: Color
deepSkyBlue4 = "38"

deepSkyBlue5 :: Color
deepSkyBlue5 = "39"

green1 :: Color
green1 = "40"

springGreen1 :: Color
springGreen1 = "41"

springGreen2 :: Color
springGreen2 = "42"

cyan :: Color
cyan = "43"

darkTurquoise :: Color
darkTurquoise = "44"

turquoise0 :: Color
turquoise0 = "45"

green2 :: Color
green2 = "46"

springGreen3 :: Color
springGreen3 = "47"

springGreen4 :: Color
springGreen4 = "48"

mediumSpringGreen :: Color
mediumSpringGreen = "49"

cyan0 :: Color
cyan0 = "50"

cyan1 :: Color
cyan1 = "51"

darkRed :: Color
darkRed = "52"

deepPink :: Color
deepPink = "53"

purple :: Color
purple = "54"

purple0 :: Color
purple0 = "55"

purple1 :: Color
purple1 = "56"

blueViolet :: Color
blueViolet = "57"

orange :: Color
orange = "58"

grey37 :: Color
grey37 = "59"

mediumPurple :: Color
mediumPurple = "60"

slateBlue :: Color
slateBlue = "61"

slateBlue0 :: Color
slateBlue0 = "62"

royalBlue :: Color
royalBlue = "63"

chartreuse :: Color
chartreuse = "64"

darkSeaGreen :: Color
darkSeaGreen = "65"

paleTurquoise :: Color
paleTurquoise = "66"

steelBlue :: Color
steelBlue = "67"

steelBlue0 :: Color
steelBlue0 = "68"

cornflowerBlue :: Color
cornflowerBlue = "69"

chartreuse0 :: Color
chartreuse0 = "70"

darkSeaGreen0 :: Color
darkSeaGreen0 = "71"

cadetBlue :: Color
cadetBlue = "72"

cadetBlue0 :: Color
cadetBlue0 = "73"

skyBlue :: Color
skyBlue = "74"

steelBlue1 :: Color
steelBlue1 = "75"

chartreuse1 :: Color
chartreuse1 = "76"

paleGreen :: Color
paleGreen = "77"

seaGreen :: Color
seaGreen = "78"

aquamarine :: Color
aquamarine = "79"

mediumTurquoise :: Color
mediumTurquoise = "80"

steelBlue2 :: Color
steelBlue2 = "81"

chartreuse2 :: Color
chartreuse2 = "82"

seaGreen0 :: Color
seaGreen0 = "83"

seaGreen1 :: Color
seaGreen1 = "84"

seaGreen2 :: Color
seaGreen2 = "85"

aquamarine0 :: Color
aquamarine0 = "86"

darkSlateGrey :: Color
darkSlateGrey = "87"

darkRed0 :: Color
darkRed0 = "88"

deepPink0 :: Color
deepPink0 = "89"

darkMagenta :: Color
darkMagenta = "90"

darkMagenta0 :: Color
darkMagenta0 = "91"

darkViolet :: Color
darkViolet = "92"

purple2 :: Color
purple2 = "93"

orange0 :: Color
orange0 = "94"

lightPink :: Color
lightPink = "95"

plum :: Color
plum = "96"

mediumPurple0 :: Color
mediumPurple0 = "97"

mediumPurple1 :: Color
mediumPurple1 = "98"

slateBlue1 :: Color
slateBlue1 = "99"

yellow :: Color
yellow = "100"

wheat :: Color
wheat = "101"

grey53 :: Color
grey53 = "102"

lightSlateGrey :: Color
lightSlateGrey = "103"

mediumPurple2 :: Color
mediumPurple2 = "104"

lightSlateBlue :: Color
lightSlateBlue = "105"

yellow0 :: Color
yellow0 = "106"

darkOliveGreen :: Color
darkOliveGreen = "107"

darkSeaGreen1 :: Color
darkSeaGreen1 = "108"

lightSkyBlue :: Color
lightSkyBlue = "109"

lightSkyBlue0 :: Color
lightSkyBlue0 = "110"

skyBlue0 :: Color
skyBlue0 = "111"

chartreuse3 :: Color
chartreuse3 = "112"

darkOliveGreen0 :: Color
darkOliveGreen0 = "113"

paleGreen0 :: Color
paleGreen0 = "114"

darkSeaGreen2 :: Color
darkSeaGreen2 = "115"

darkSlateGrey0 :: Color
darkSlateGrey0 = "116"

skyBlue1 :: Color
skyBlue1 = "117"

chartreuse4 :: Color
chartreuse4 = "118"

lightGreen :: Color
lightGreen = "119"

lightGreen0 :: Color
lightGreen0 = "120"

paleGreen1 :: Color
paleGreen1 = "121"

aquamarine1 :: Color
aquamarine1 = "122"

darkSlateGrey1 :: Color
darkSlateGrey1 = "123"

red :: Color
red = "124"

deepPink1 :: Color
deepPink1 = "125"

mediumVioletRed :: Color
mediumVioletRed = "126"

magenta :: Color
magenta = "127"

darkViolet0 :: Color
darkViolet0 = "128"

purple3 :: Color
purple3 = "129"

darkOrange :: Color
darkOrange = "130"

indianRed :: Color
indianRed = "131"

hotPink :: Color
hotPink = "132"

mediumOrchid :: Color
mediumOrchid = "133"

mediumOrchid0 :: Color
mediumOrchid0 = "134"

mediumPurple3 :: Color
mediumPurple3 = "135"

darkGoldenrod :: Color
darkGoldenrod = "136"

lightSalmon :: Color
lightSalmon = "137"

rosyBrown :: Color
rosyBrown = "138"

grey63 :: Color
grey63 = "139"

mediumPurple4 :: Color
mediumPurple4 = "140"

mediumPurple5 :: Color
mediumPurple5 = "141"

gold :: Color
gold = "142"

darkKhaki :: Color
darkKhaki = "143"

navajoWhite :: Color
navajoWhite = "144"

grey69 :: Color
grey69 = "145"

lightSteelBlue :: Color
lightSteelBlue = "146"

lightSteelBlue0 :: Color
lightSteelBlue0 = "147"

yellow1 :: Color
yellow1 = "148"

darkOliveGreen1 :: Color
darkOliveGreen1 = "149"

darkSeaGreen3 :: Color
darkSeaGreen3 = "150"

darkSeaGreen4 :: Color
darkSeaGreen4 = "151"

lightCyan :: Color
lightCyan = "152"

lightSkyBlue1 :: Color
lightSkyBlue1 = "153"

greenYellow :: Color
greenYellow = "154"

darkOliveGreen2 :: Color
darkOliveGreen2 = "155"

paleGreen2 :: Color
paleGreen2 = "156"

darkSeaGreen5 :: Color
darkSeaGreen5 = "157"

darkSeaGreen6 :: Color
darkSeaGreen6 = "158"

paleTurquoise0 :: Color
paleTurquoise0 = "159"

red0 :: Color
red0 = "160"

deepPink2 :: Color
deepPink2 = "161"

deepPink3 :: Color
deepPink3 = "162"

magenta0 :: Color
magenta0 = "163"

magenta1 :: Color
magenta1 = "164"

magenta2 :: Color
magenta2 = "165"

darkOrange0 :: Color
darkOrange0 = "166"

indianRed0 :: Color
indianRed0 = "167"

hotPink0 :: Color
hotPink0 = "168"

hotPink1 :: Color
hotPink1 = "169"

orchid :: Color
orchid = "170"

mediumOrchid1 :: Color
mediumOrchid1 = "171"

orange1 :: Color
orange1 = "172"

lightSalmon0 :: Color
lightSalmon0 = "173"

lightPink0 :: Color
lightPink0 = "174"

pink :: Color
pink = "175"

plum0 :: Color
plum0 = "176"

violet :: Color
violet = "177"

gold0 :: Color
gold0 = "178"

lightGoldenrod :: Color
lightGoldenrod = "179"

tan0 :: Color
tan0 = "180"

mistyRose :: Color
mistyRose = "181"

thistle :: Color
thistle = "182"

plum1 :: Color
plum1 = "183"

yellow2 :: Color
yellow2 = "184"

khaki :: Color
khaki = "185"

lightGoldenrod0 :: Color
lightGoldenrod0 = "186"

lightYellow :: Color
lightYellow = "187"

grey84 :: Color
grey84 = "188"

lightSteelBlue1 :: Color
lightSteelBlue1 = "189"

yellow3 :: Color
yellow3 = "190"

darkOliveGreen3 :: Color
darkOliveGreen3 = "191"

darkOliveGreen4 :: Color
darkOliveGreen4 = "192"

darkSeaGreen7 :: Color
darkSeaGreen7 = "193"

honeydew :: Color
honeydew = "194"

lightCyan0 :: Color
lightCyan0 = "195"

red1 :: Color
red1 = "196"

deepPink4 :: Color
deepPink4 = "197"

deepPink5 :: Color
deepPink5 = "198"

deepPink6 :: Color
deepPink6 = "199"

magenta3 :: Color
magenta3 = "200"

magenta4 :: Color
magenta4 = "201"

orangeRed :: Color
orangeRed = "202"

indianRed1 :: Color
indianRed1 = "203"

indianRed2 :: Color
indianRed2 = "204"

hotPink2 :: Color
hotPink2 = "205"

hotPink3 :: Color
hotPink3 = "206"

mediumOrchid2 :: Color
mediumOrchid2 = "207"

darkOrange1 :: Color
darkOrange1 = "208"

salmon :: Color
salmon = "209"

lightCoral :: Color
lightCoral = "210"

paleVioletRed :: Color
paleVioletRed = "211"

orchid0 :: Color
orchid0 = "212"

orchid1 :: Color
orchid1 = "213"

orange2 :: Color
orange2 = "214"

sandyBrown :: Color
sandyBrown = "215"

lightSalmon1 :: Color
lightSalmon1 = "216"

lightPink1 :: Color
lightPink1 = "217"

pink0 :: Color
pink0 = "218"

plum2 :: Color
plum2 = "219"

gold1 :: Color
gold1 = "220"

lightGoldenrod1 :: Color
lightGoldenrod1 = "221"

lightGoldenrod2 :: Color
lightGoldenrod2 = "222"

navajoWhite0 :: Color
navajoWhite0 = "223"

mistyRose0 :: Color
mistyRose0 = "224"

thistle0 :: Color
thistle0 = "225"

yellow4 :: Color
yellow4 = "226"

lightGoldenrod3 :: Color
lightGoldenrod3 = "227"

khaki0 :: Color
khaki0 = "228"

wheat0 :: Color
wheat0 = "229"

cornsilk :: Color
cornsilk = "230"

grey100 :: Color
grey100 = "231"

grey3 :: Color
grey3 = "232"

grey7 :: Color
grey7 = "233"

grey11 :: Color
grey11 = "234"

grey15 :: Color
grey15 = "235"

grey19 :: Color
grey19 = "236"

grey23 :: Color
grey23 = "237"

grey27 :: Color
grey27 = "238"

grey30 :: Color
grey30 = "239"

grey35 :: Color
grey35 = "240"

grey39 :: Color
grey39 = "241"

grey42 :: Color
grey42 = "242"

grey46 :: Color
grey46 = "243"

grey50 :: Color
grey50 = "244"

grey54 :: Color
grey54 = "245"

grey58 :: Color
grey58 = "246"

grey62 :: Color
grey62 = "247"

grey66 :: Color
grey66 = "248"

grey70 :: Color
grey70 = "249"

grey74 :: Color
grey74 = "250"

grey78 :: Color
grey78 = "251"

grey82 :: Color
grey82 = "252"

grey85 :: Color
grey85 = "253"

grey89 :: Color
grey89 = "254"

grey93 :: Color
grey93 = "255"

