static int borderpx = 10;

static float cwscale = 1.0;
static float chscale = 1.0;

// boxdraw
const int boxdraw = 1;
const int boxdraw_bold = 1;
const int boxdraw_braille = 0;

static char ascii_printable[] = " !\"#$%&'()*+,-./0123456789:;<=>?"
                                "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
                                "`abcdefghijklmnopqrstuvwxyz{|}~";
wchar_t *worddelimiters = L" ";
char *stty_args = "stty raw pass8 nl -echo -iexten -cstopb 38400";
char *termname = "st-256color";
char *vtiden = "\033[?12;4c";

// undercurl
#define UNDERCURL_STYLE 0

static char *shell = "/bin/sh";
char *scroll = NULL;
char *utmp = NULL;

static unsigned int defaultattr = 11;
unsigned int defaultrcs = 257;
static int bellvolume = 0;
static double minlatency = 8;
static double maxlatency = 16;
unsigned int tabspaces = 8;
static uint su_timeout = 100;

static unsigned int blinktimeout = 800;
static unsigned int cursorshape = 2;
static unsigned int cursorthickness = 2;
static unsigned int cols = 80;
static unsigned int rows = 24;

// blinking cursor
static unsigned int cursorstyle = 5; // blinking bar
static Rune stcursor = 0x2603;       // snowman

int allowaltscreen = 1;
int allowwindowops = 0;

unsigned int defaultbg = 258;
unsigned int defaultfg = 259;
unsigned int defaultcs = 256;

#define TERMMOD (ControlMask | ShiftMask)

static uint ignoremod = Mod2Mask | XK_SWITCH_MOD;

static Shortcut shortcuts[] = {
    {ControlMask, XK_equal, zoom, {.f = +1}},
    {ControlMask, XK_minus, zoom, {.f = -1}},
    {TERMMOD, XK_equal, zoomreset, {.f = 0}},
    {TERMMOD, XK_C, clipcopy, {.i = 0}},
    {TERMMOD, XK_V, clippaste, {.i = 0}},
    {TERMMOD, XK_K, kscrollup, {.i = -1}, S_PRI},
    {TERMMOD, XK_J, kscrolldown, {.i = -1}, S_PRI},
};

static uint forcemousemod = ShiftMask;

static MouseShortcut mshortcuts[] = {
    {XK_ANY_MOD, Button2, selpaste, {.i = 0}, 1},
    {ShiftMask, Button4, ttysend, {.s = "\033[5;2~"}},
    {ShiftMask, Button5, ttysend, {.s = "\033[6;2~"}},
    {XK_NO_MOD, Button4, kscrollup, {.i = 1}, 0, S_PRI},
    {XK_NO_MOD, Button5, kscrolldown, {.i = 1}, 0, S_PRI},
    {XK_ANY_MOD, Button4, ttysend, {.s = "\031"}, 0, S_ALT},
    {XK_ANY_MOD, Button5, ttysend, {.s = "\005"}, 0, S_ALT},
};

static uint selmasks[] = {
    [SEL_RECTANGULAR] = Mod1Mask,
};

static unsigned int doubleclicktimeout = 300;
static unsigned int tripleclicktimeout = 600;

static unsigned int mouseshape = XC_xterm;
static unsigned int mousefg = 7;
static unsigned int mousebg = 0;

// open url on click
static uint url_opener_modkey = XK_ANY_MOD;
static char *url_opener = "xdg-open";
