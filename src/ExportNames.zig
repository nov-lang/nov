const std = @import("std");

// C11 math/complex functions have double, float and long-double spellings.
const math_functions =
    "acos asin atan atan2 cos sin tan acosh asinh atanh cosh sinh tanh " ++
    "exp exp2 expm1 frexp ilogb ldexp log log10 log1p log2 logb modf scalbn scalbln " ++
    "cbrt fabs hypot pow sqrt erf erfc lgamma tgamma ceil floor nearbyint rint lrint llrint " ++
    "round lround llround trunc fmod remainder remquo copysign nan nextafter nexttoward fdim fmax fmin fma";
const complex_functions =
    "cacos casin catan ccos csin ctan cacosh casinh catanh ccosh csinh ctanh " ++
    "cexp clog cabs cpow csqrt carg cimag conj cproj creal " ++
    // C11 7.31.1 reserves these for future complex-library additions as well.
    "cerf cerfc cexp2 cexpm1 clog10 clog1p clog2 clgamma ctgamma";

fn floatingName(name: []const u8, functions: []const u8) bool {
    var names = std.mem.tokenizeScalar(u8, functions, ' ');
    while (names.next()) |base| {
        if (std.mem.eql(u8, name, base)) return true;
        if (name.len == base.len + 1 and std.mem.startsWith(u8, name, base) and (name[base.len] == 'f' or name[base.len] == 'l')) return true;
    }
    return false;
}

/// Declaration names may refer to C library functions; definitions may not.
/// Both forms must avoid C tokens/types/macros and generated-code collisions.
pub fn validExternName(name: []const u8) bool {
    if (name.len == 0 or !std.ascii.isAlphabetic(name[0])) return false;
    for (name[1..]) |c| if (!std.ascii.isAlphanumeric(c) and c != '_') return false;
    // Function references also appear in scopes containing generated parameters
    // and temporaries; an external symbol must not be shadowed by those locals.
    if (name.len > 1 and (name[0] == 'p' or name[0] == 'v')) {
        var numbered = true;
        for (name[1..]) |c| if (!std.ascii.isDigit(c)) {
            numbered = false;
            break;
        };
        if (numbered) return false;
    }
    for ([_][]const u8{ "nov_rt_", "nov_gc_", "nov_fn_", "nov_record_", "nov_range_", "nov_string", "nov_array", "nov_M", "nov_element_", "nov_to_string_", "NOV_GC_" }) |prefix| {
        if (std.mem.startsWith(u8, name, prefix)) return false;
    }
    // float.h is emitted for reachable floats, including signature-only uses.
    // Reserve exact C11 macro names, not unrelated FLT_/DBL_/LDBL_ user names.
    for ([_][]const u8{ "FLT_RADIX", "FLT_ROUNDS", "FLT_EVAL_METHOD", "DECIMAL_DIG" }) |macro| {
        if (std.mem.eql(u8, name, macro)) return false;
    }
    inline for (.{ "FLT", "DBL", "LDBL" }) |prefix| {
        inline for (.{ "MANT_DIG", "DIG", "MIN_EXP", "MIN_10_EXP", "MAX_EXP", "MAX_10_EXP", "MAX", "EPSILON", "MIN", "TRUE_MIN", "HAS_SUBNORM", "DECIMAL_DIG" }) |suffix| {
            if (std.mem.eql(u8, name, prefix ++ "_" ++ suffix)) return false;
        }
    }
    // Integer limits, constructors and format macros from stdint/inttypes.
    // Do not reserve unrelated user names such as INTERFACE or PRIVATE.
    inline for (.{ "8", "16", "32", "64", "MAX", "PTR", "_LEAST8", "_LEAST16", "_LEAST32", "_LEAST64", "_FAST8", "_FAST16", "_FAST32", "_FAST64" }) |width| {
        inline for (.{ "INT", "UINT" }) |prefix| {
            inline for (.{ "_MIN", "_MAX", "_C" }) |suffix| {
                if (std.mem.eql(u8, name, prefix ++ width ++ suffix)) return false;
            }
        }
    }
    inline for (.{ "8", "16", "32", "64", "MAX", "PTR", "LEAST8", "LEAST16", "LEAST32", "LEAST64", "FAST8", "FAST16", "FAST32", "FAST64" }) |width| {
        inline for (.{ "PRId", "PRIi", "PRIo", "PRIu", "PRIx", "PRIX", "SCNd", "SCNi", "SCNo", "SCNu", "SCNx" }) |prefix| {
            if (std.mem.eql(u8, name, prefix ++ width)) return false;
        }
    }
    if ((std.mem.startsWith(u8, name, "int") or std.mem.startsWith(u8, name, "uint")) and std.mem.endsWith(u8, name, "_t")) return false;
    const reserved =
        "nov_frame nov_roots nov_target main auto break case char const continue default do double else enum extern float for goto if inline int long register restrict return short signed sizeof static struct switch typedef union unsigned void volatile while " ++
        "alignas alignof bool true false static_assert thread_local " ++
        "CHAR_BIT SCHAR_MIN SCHAR_MAX UCHAR_MAX CHAR_MIN CHAR_MAX MB_LEN_MAX SHRT_MIN SHRT_MAX USHRT_MAX INT_MIN INT_MAX UINT_MAX LONG_MIN LONG_MAX ULONG_MAX LLONG_MIN LLONG_MAX ULLONG_MAX " ++
        "SIZE_MAX PTRDIFF_MIN PTRDIFF_MAX SIG_ATOMIC_MIN SIG_ATOMIC_MAX WCHAR_MIN WCHAR_MAX WINT_MIN WINT_MAX " ++
        "NULL size_t ptrdiff_t wchar_t max_align_t va_list offsetof FILE fpos_t EOF BUFSIZ FOPEN_MAX FILENAME_MAX L_tmpnam SEEK_CUR SEEK_END SEEK_SET TMP_MAX stderr stdin stdout " ++
        "EXIT_FAILURE EXIT_SUCCESS RAND_MAX MB_CUR_MAX div_t ldiv_t lldiv_t imaxdiv_t";
    var names = std.mem.tokenizeScalar(u8, reserved, ' ');
    while (names.next()) |word| if (std.mem.eql(u8, word, name)) return false;
    return true;
}

/// Portable C11 definitions additionally avoid all standard-library external names
/// and future library reservations, regardless of which headers are emitted.
pub fn validName(name: []const u8) bool {
    if (!validExternName(name)) return false;
    if (floatingName(name, math_functions) or floatingName(name, complex_functions)) return false;
    for ([_][]const u8{ "atomic_", "cnd_", "mtx_", "thrd_", "tss_" }) |prefix| {
        if (std.mem.startsWith(u8, name, prefix)) return false;
    }
    // C11 future library directions reserve these external-name families even
    // without including the corresponding header (7.1.3 and 7.31).
    for ([_][]const u8{ "is", "to", "str", "mem", "wcs", "fe" }) |prefix| {
        if (name.len > prefix.len and std.mem.startsWith(u8, name, prefix) and std.ascii.isLower(name[prefix.len])) return false;
    }
    const reserved =
        "nov_frame nov_roots nov_target main auto break case char const continue default do double else enum extern float for goto if inline int long register restrict return short signed sizeof static struct switch typedef union unsigned void volatile while " ++
        "alignas alignof bool true false static_assert thread_local " ++
        "CHAR_BIT SCHAR_MIN SCHAR_MAX UCHAR_MAX CHAR_MIN CHAR_MAX MB_LEN_MAX SHRT_MIN SHRT_MAX USHRT_MAX INT_MIN INT_MAX UINT_MAX LONG_MIN LONG_MAX ULONG_MAX LLONG_MIN LLONG_MAX ULLONG_MAX " ++
        "SIZE_MAX PTRDIFF_MIN PTRDIFF_MAX SIG_ATOMIC_MIN SIG_ATOMIC_MAX WCHAR_MIN WCHAR_MAX WINT_MIN WINT_MAX " ++
        "NULL size_t ptrdiff_t wchar_t max_align_t va_list offsetof FILE fpos_t EOF BUFSIZ FOPEN_MAX FILENAME_MAX L_tmpnam SEEK_CUR SEEK_END SEEK_SET TMP_MAX stderr stdin stdout " ++
        "remove rename tmpfile tmpnam fclose fflush fopen freopen setbuf setvbuf fprintf fscanf printf scanf snprintf sprintf sscanf vfprintf vfscanf vprintf vscanf vsnprintf vsprintf vsscanf fgetc fgets fputc fputs getc getchar gets putc putchar puts ungetc fread fwrite fgetpos fseek fsetpos ftell rewind clearerr feof ferror perror " ++
        "EXIT_FAILURE EXIT_SUCCESS RAND_MAX MB_CUR_MAX div_t ldiv_t lldiv_t atof atoi atol atoll strtod strtof strtold strtol strtoll strtoul strtoull rand srand aligned_alloc calloc free malloc realloc abort atexit at_quick_exit exit quick_exit getenv system bsearch qsort abs labs llabs div ldiv lldiv mblen mbtowc wctomb mbstowcs wcstombs " ++
        "memcpy memmove strcpy strncpy strcat strncat memcmp strcmp strcoll strncmp strxfrm memchr strchr strcspn strpbrk strrchr strspn strstr strtok memset strerror strlen " ++
        "imaxdiv_t imaxabs imaxdiv strtoimax strtoumax wcstoimax wcstoumax " ++
        "feclearexcept fegetexceptflag feraiseexcept fesetexceptflag fetestexcept fegetround fesetround fegetenv feholdexcept fesetenv feupdateenv fenv_t fexcept_t " ++
        "errno localeconv setlocale lconv signal raise sig_atomic_t setjmp longjmp jmp_buf " ++
        "clock difftime mktime time asctime ctime gmtime localtime strftime timespec_get clock_t time_t tm timespec " ++
        "mbrtoc16 c16rtomb mbrtoc32 c32rtomb char16_t char32_t " ++
        "btowc wctob mbsinit mbrlen mbrtowc wcrtomb mbsrtowcs wcsrtombs mbstate_t " ++
        "fwide fwprintf fwscanf swprintf swscanf vfwprintf vfwscanf vswprintf vswscanf vwprintf vwscanf wprintf wscanf " ++
        "fgetwc fgetws fputwc fputws getwc getwchar putwc putwchar ungetwc " ++
        "wmemchr wmemcmp wmemcpy wmemmove wmemset wctype wctrans wctype_t wctrans_t wint_t " ++
        "call_once once_flag kill_dependency " ++
        "fpclassify signbit math_errhandling HUGE_VAL HUGE_VALF HUGE_VALL INFINITY NAN " ++
        // Annex K bounds-checking interfaces, when supported by the C library.
        "set_constraint_handler_s abort_handler_s ignore_handler_s constraint_handler_t errno_t rsize_t " ++
        "fopen_s freopen_s tmpfile_s tmpnam_s fprintf_s fscanf_s printf_s scanf_s snprintf_s sprintf_s sscanf_s " ++
        "vfprintf_s vfscanf_s vprintf_s vscanf_s vsnprintf_s vsprintf_s vsscanf_s gets_s " ++
        "getenv_s bsearch_s qsort_s wctomb_s mbstowcs_s wcstombs_s " ++
        "asctime_s ctime_s gmtime_s localtime_s " ++
        "fwprintf_s fwscanf_s swprintf_s swscanf_s snwprintf_s vfwprintf_s vfwscanf_s vswprintf_s vswscanf_s vsnwprintf_s vwprintf_s vwscanf_s wprintf_s wscanf_s " ++
        "wmemcpy_s wmemmove_s wcrtomb_s mbsrtowcs_s wcsrtombs_s";
    var names = std.mem.tokenizeScalar(u8, reserved, ' ');
    while (names.next()) |word| if (std.mem.eql(u8, word, name)) return false;
    return true;
}

test "foreign names reject all C11 float header macros in every precision" {
    const macros =
        "FLT_RADIX FLT_ROUNDS FLT_EVAL_METHOD DECIMAL_DIG " ++
        "FLT_MANT_DIG FLT_DIG FLT_MIN_EXP FLT_MIN_10_EXP FLT_MAX_EXP FLT_MAX_10_EXP FLT_MAX FLT_EPSILON FLT_MIN FLT_TRUE_MIN FLT_HAS_SUBNORM FLT_DECIMAL_DIG " ++
        "DBL_MANT_DIG DBL_DIG DBL_MIN_EXP DBL_MIN_10_EXP DBL_MAX_EXP DBL_MAX_10_EXP DBL_MAX DBL_EPSILON DBL_MIN DBL_TRUE_MIN DBL_HAS_SUBNORM DBL_DECIMAL_DIG " ++
        "LDBL_MANT_DIG LDBL_DIG LDBL_MIN_EXP LDBL_MIN_10_EXP LDBL_MAX_EXP LDBL_MAX_10_EXP LDBL_MAX LDBL_EPSILON LDBL_MIN LDBL_TRUE_MIN LDBL_HAS_SUBNORM LDBL_DECIMAL_DIG";
    var names = std.mem.tokenizeScalar(u8, macros, ' ');
    while (names.next()) |name| {
        try std.testing.expect(!validExternName(name));
        try std.testing.expect(!validName(name));
    }
    for ([_][]const u8{ "FLT", "DBL", "LDBL", "FLT_MAX_value", "DBL_MANT_DIG_value", "LDBL_custom", "DECIMAL_DIGITS" }) |name| {
        try std.testing.expect(validExternName(name));
        try std.testing.expect(validName(name));
    }
}

test "foreign names reject integer least and fast limit macros in every width" {
    inline for (.{ "INT", "UINT" }) |prefix| {
        inline for (.{ "_LEAST", "_FAST" }) |family| {
            inline for (.{ "8", "16", "32", "64" }) |width| {
                inline for (.{ "_MIN", "_MAX" }) |suffix| {
                    const name = prefix ++ family ++ width ++ suffix;
                    try std.testing.expect(!validExternName(name));
                    try std.testing.expect(!validName(name));
                }
            }
        }
    }
}

test "extern names permit library functions but reject C tokens and generated locals" {
    for ([_][]const u8{ "abs", "putchar", "rand", "exit", "sin", "nov_add", "custom_func", "memcustom" }) |name| try std.testing.expect(validExternName(name));
    for ([_][]const u8{ "", "_private", "9bad", "a-b", "a\x00b", "é", "main", "while", "bool", "true", "FILE", "EOF", "size_t", "int32_t", "UINT64_MAX", "PRId32", "nov_rt_x", "nov_gc_x", "nov_fn_x", "p0", "v123", "nov_roots", "nov_frame", "nov_target" }) |name| {
        try std.testing.expect(!validExternName(name));
    }
}

test "export names reject C11 math complex and future functions in every precision" {
    for ([_][]const u8{ math_functions, complex_functions }) |functions| {
        var names = std.mem.tokenizeScalar(u8, functions, ' ');
        while (names.next()) |name| {
            try std.testing.expect(!validName(name));
            for ([_]u8{ 'f', 'l' }) |suffix| {
                var buffer: [64]u8 = undefined;
                const variant = try std.fmt.bufPrint(&buffer, "{s}{c}", .{ name, suffix });
                try std.testing.expect(!validName(variant));
            }
        }
    }
}

test "export names reject all C11 library categories without including their headers" {
    for ([_][]const u8{
        "sin",                      "sqrt",              "time",             "signal",          "raise",        "setjmp",     "longjmp",
        "feclearexcept",            "fegetexceptflag",   "feraiseexcept",    "fesetexceptflag", "fetestexcept", "fegetround", "fesetround",
        "fegetenv",                 "feholdexcept",      "fesetenv",         "feupdateenv",     "localeconv",   "setlocale",  "clock",
        "difftime",                 "mktime",            "asctime",          "ctime",           "gmtime",       "localtime",  "strftime",
        "timespec_get",             "mbrtoc16",          "c16rtomb",         "mbrtoc32",        "c32rtomb",     "btowc",      "wctob",
        "mbsinit",                  "mbrlen",            "mbrtowc",          "wcrtomb",         "mbsrtowcs",    "wcsrtombs",  "fwide",
        "fwprintf",                 "fwscanf",           "swprintf",         "swscanf",         "vfwprintf",    "vfwscanf",   "vswprintf",
        "vswscanf",                 "vwprintf",          "vwscanf",          "wprintf",         "wscanf",       "fgetwc",     "fgetws",
        "fputwc",                   "fputws",            "getwc",            "getwchar",        "putwc",        "putwchar",   "ungetwc",
        "wmemchr",                  "wmemcmp",           "wmemcpy",          "wmemmove",        "wmemset",      "wctype",     "wctrans",
        "towctrans",                "iswctype",          "cnd_init",         "mtx_lock",        "thrd_create",  "tss_get",    "call_once",
        "atomic_load",              "atomic_flag_clear", "kill_dependency",  "iscustom",        "tocustom",     "strcustom",  "memcustom",
        "wcscustom",                "fecustom",          "cnd_future",       "mtx_future",      "thrd_future",  "tss_future", "atomic_future",
        "set_constraint_handler_s", "abort_handler_s",   "ignore_handler_s", "fopen_s",         "printf_s",     "qsort_s",    "localtime_s",
        "wmemcpy_s",
    }) |name| try std.testing.expect(!validName(name));
    for ([_][]const u8{ "add", "invert", "select", "nov_add", "sinusoid", "sqrt_value", "timer", "signaller", "is", "to", "str", "mem", "wcs", "fe" }) |name| try std.testing.expect(validName(name));
}

test "export names exclude C headers and backend namespaces without reserving nov" {
    for ([_][]const u8{ "add", "nov_add", "answer42", "A_b", "INTERFACE", "PRIVATE" }) |name| try std.testing.expect(validName(name));
    for ([_][]const u8{ "", "_private", "9bad", "a-b", "é", "main", "while", "bool", "true", "INT64_MAX", "UINT8_C", "PRId64", "SCNuMAX", "uint_fast32_t", "FILE", "EOF", "size_t", "NULL", "stdout", "malloc", "memcpy", "imaxdiv_t", "nov_rt_x", "nov_gc_x", "nov_fn_x", "nov_record_x", "nov_range_x", "nov_stringFoo", "nov_arrayFoo", "nov_M0", "p0", "v12", "nov_frame", "nov_roots", "nov_target", "nov_element_0", "nov_element_offsets_0", "nov_to_string_0", "NOV_GC_MALLOC", "NOV_GC_H", "offsetof", "strcustom", "memcustom", "wcscustom" }) |name| try std.testing.expect(!validName(name));
}
