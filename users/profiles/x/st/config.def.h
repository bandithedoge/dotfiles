static unsigned int blinktimeout = 800;
static int borderpx = 10;
static unsigned int cursorthickness = 2;
static int bellvolume = 0;
static unsigned int cols = 80;
static unsigned int rows = 24;

unsigned int defaultbg = 258;
unsigned int defaultfg = 259;
unsigned int defaultcs = 256;
unsigned int defaultrcs = 257;
static unsigned int defaultattr = 11;

static unsigned int mouseshape = XC_xterm;
static unsigned int mousefg = 7;
static unsigned int mousebg = 0;

char *utmp = NULL;
char *scroll = NULL;
static char *shell = "/bin/sh";
char *termname = "st-256color";
unsigned int tabspaces = 8;

char *stty_args = "stty raw pass8 nl -echo -iexten -cstopb 38400";
char *vtiden = "\033[?12;4c";

wchar_t *worddelimiters = L" ";
static char ascii_printable[] = " !\"#$%&'()*+,-./0123456789:;<=>?"
                                "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
                                "`abcdefghijklmnopqrstuvwxyz{|}~";

static float cwscale = 1.0;
static float chscale = 1.0;

static unsigned int doubleclicktimeout = 300;
static unsigned int tripleclicktimeout = 600;

int allowaltscreen = 1;
int allowwindowops = 0;

static double minlatency = 8;
static double maxlatency = 33;

static uint forcemousemod = ShiftMask;
static uint ignoremod = Mod2Mask | XK_SWITCH_MOD;

static MouseShortcut mshortcuts[] = {
    {XK_ANY_MOD, Button2, clippaste, {.i = 0}, 1},
    {ShiftMask, Button4, ttysend, {.s = "\033[5;2~"}},
    {ShiftMask, Button5, ttysend, {.s = "\033[6;2~"}},
    {XK_ANY_MOD, Button4, ttysend, {.s = "\031"}},
    {XK_ANY_MOD, Button5, ttysend, {.s = "\005"}},
};

#define MODKEY Mod1Mask
#define TERMMOD (ControlMask | ShiftMask)

static Shortcut shortcuts[] = {
    {XK_ANY_MOD, XK_Break, sendbreak, {.i = 0}},
    {ControlMask, XK_Print, toggleprinter, {.i = 0}},
    {ShiftMask, XK_Print, printscreen, {.i = 0}},
    {XK_ANY_MOD, XK_Print, printsel, {.i = 0}},
    {TERMMOD, XK_Prior, zoom, {.f = +1}},
    {TERMMOD, XK_Next, zoom, {.f = -1}},
    {TERMMOD, XK_Home, zoomreset, {.f = 0}},
    {TERMMOD, XK_C, clipcopy, {.i = 0}},
    {TERMMOD, XK_V, clippaste, {.i = 0}},
    {TERMMOD, XK_Y, clippaste, {.i = 0}},
    {ShiftMask, XK_Insert, clippaste, {.i = 0}},
    {TERMMOD, XK_Num_Lock, numlock, {.i = 0}},
    {MODKEY, XK_c, normalMode, {.i = 0}},
};

static uint selmasks[] = {
    [SEL_RECTANGULAR] = Mod1Mask,
};

// open url on click
static char *url_opener = "xdg-open";

// sync patch
static uint su_timeout = 200;

// boxdraw
const int boxdraw = 1;
const int boxdraw_bold = 1;
const int boxdraw_braille = 1;

// vim browse
unsigned int const currentBg = 0, buffSize = 2048;
int const mouseYank = 1, mouseSelect = 0;
unsigned int const highlightBg = 15, highlightFg = 258;
char const wDelS[] = "!\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~", wDelL[] = " \t";
char *nmKeys[] = {};
unsigned int const amountNmKeys = sizeof(nmKeys) / sizeof(*nmKeys);
/// Style of the {command, search} string shown in the right corner (y,v,V,/)
Glyph styleSearch = {' ', ATTR_ITALIC, 0, 15};
Glyph style[] = {{' ', ATTR_ITALIC, 0, 15},
                 {' ', ATTR_ITALIC, 15, 11},
                 {' ', ATTR_ITALIC, 15, 4},
                 {' ', ATTR_ITALIC, 8, 12}};

// blinking cursor
static unsigned int cursorstyle = 5;
static Rune stcursor = 0x2603; /* snowman (U+2603) */

// undercurl
/**
 * 0
 *  _   _   _   _
 * ( ) ( ) ( ) ( )
 *	 (_) (_) (_) (_)
 *
 * 1
 * /\  /\   /\	/\
 *   \/  \/	  \/
 *
 * 2
 *	_     _     _
 * / \   / \   / \
 *    \_/   \_/
 */
#define UNDERCURL_STYLE 0
