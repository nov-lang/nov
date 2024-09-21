// MIT License
//
// Copyright © 2021-present Benoit Giannangeli
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// https://github.com/buzz-language/buzz/blob/main/src/linenoise.zig

pub const linenoiseState = opaque {
    // Non blocking API.
    pub extern fn linenoiseEditStart(stdind_fd: c_int, stdout_fd: c_int, buf: [*]u8, buflen: usize, prompt: [*:0]const u8) c_int;
    pub extern fn linenoiseEditFeed() [*:0]u8;
    pub extern fn linenoiseEditStop() void;
    pub extern fn linenoiseHide() void;
    pub extern fn linenoiseShow() void;
};

pub const linenoiseCompletions = extern struct {
    len: usize,
    cvec: *[*:0]u8,
};

// Blocking API.
pub extern fn linenoise(prompt: [*:0]const u8) ?[*:0]const u8;

// Completion API.
pub const linenoiseCompletionCallback = fn ([*:0]const u8, *linenoiseCompletions) void;
pub const linenoiseHintsCallback = fn ([*:0]const u8, *c_int, *c_int) [*:0]const u8;
pub const linenoiseFreeHintsCallback = fn (*anyopaque) void;
pub extern fn linenoiseSetCompletionCallback(callback: *linenoiseCompletionCallback) void;
pub extern fn linenoiseSetHintsCallback(callback: *linenoiseHintsCallback) void;
pub extern fn linenoiseSetFreeHintsCallback(callback: *linenoiseFreeHintsCallback) void;
pub extern fn linenoiseAddCompletion(completions: *linenoiseCompletions, completion: [*:0]const u8) void;

// History API.
pub extern fn linenoiseHistoryAdd(line: [*:0]const u8) c_int;
pub extern fn linenoiseHistorySetMaxLen(len: c_int) c_int;
pub extern fn linenoiseHistorySave(filename: [*:0]const u8) c_int;
pub extern fn linenoiseHistoryLoad(filename: [*:0]const u8) c_int;

// Other utilities.
pub extern fn linenoiseClearScreen() void;
pub extern fn linenoiseSetMultiLine(ml: c_int) void;
pub extern fn linenoisePrintKeyCodes() void;
pub extern fn linenoiseMaskModeEnable() void;
pub extern fn linenoiseMaskModeDisable() void;
