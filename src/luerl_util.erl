%% Copyright (c) 2019 Robert Virding
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% File    : luerl_util.erl
%% Purpose : Utility functions for Luer.

-module(luerl_util).

-include("luerl.hrl").

?MODULEDOC(false).

-export([errname_info/1]).

%% Convert error names to errnos and strings.
errname_info(Name) ->
    #{errno => get_errno(Name),
      errstr => erl_posix_msg:message(Name)}.

%% Made using the following command (`errno' from the moreutils package):
%% `errno -l | sort -k2 -n | awk '{print "get_errno("tolower($1)") -> "$2";"}''
get_errno(eperm) -> 1;
get_errno(enoent) -> 2;
get_errno(esrch) -> 3;
get_errno(eintr) -> 4;
get_errno(eio) -> 5;
get_errno(enxio) -> 6;
get_errno(e2big) -> 7;
get_errno(enoexec) -> 8;
get_errno(ebadf) -> 9;
get_errno(echild) -> 10;
get_errno(eagain) -> 11;
get_errno(ewouldblock) -> 11;
get_errno(enomem) -> 12;
get_errno(eacces) -> 13;
get_errno(efault) -> 14;
get_errno(enotblk) -> 15;
get_errno(ebusy) -> 16;
get_errno(eexist) -> 17;
get_errno(exdev) -> 18;
get_errno(enodev) -> 19;
get_errno(enotdir) -> 20;
get_errno(eisdir) -> 21;
get_errno(einval) -> 22;
get_errno(enfile) -> 23;
get_errno(emfile) -> 24;
get_errno(enotty) -> 25;
get_errno(etxtbsy) -> 26;
get_errno(efbig) -> 27;
get_errno(enospc) -> 28;
get_errno(espipe) -> 29;
get_errno(erofs) -> 30;
get_errno(emlink) -> 31;
get_errno(epipe) -> 32;
get_errno(edom) -> 33;
get_errno(erange) -> 34;
get_errno(edeadlk) -> 35;
get_errno(edeadlock) -> 35;
get_errno(enametoolong) -> 36;
get_errno(enolck) -> 37;
get_errno(enosys) -> 38;
get_errno(enotempty) -> 39;
get_errno(eloop) -> 40;
get_errno(enomsg) -> 42;
get_errno(eidrm) -> 43;
get_errno(echrng) -> 44;
get_errno(el2nsync) -> 45;
get_errno(el3hlt) -> 46;
get_errno(el3rst) -> 47;
get_errno(elnrng) -> 48;
get_errno(eunatch) -> 49;
get_errno(enocsi) -> 50;
get_errno(el2hlt) -> 51;
get_errno(ebade) -> 52;
get_errno(ebadr) -> 53;
get_errno(exfull) -> 54;
get_errno(enoano) -> 55;
get_errno(ebadrqc) -> 56;
get_errno(ebadslt) -> 57;
get_errno(ebfont) -> 59;
get_errno(enostr) -> 60;
get_errno(enodata) -> 61;
get_errno(etime) -> 62;
get_errno(enosr) -> 63;
get_errno(enonet) -> 64;
get_errno(enopkg) -> 65;
get_errno(eremote) -> 66;
get_errno(enolink) -> 67;
get_errno(eadv) -> 68;
get_errno(esrmnt) -> 69;
get_errno(ecomm) -> 70;
get_errno(eproto) -> 71;
get_errno(emultihop) -> 72;
get_errno(edotdot) -> 73;
get_errno(ebadmsg) -> 74;
get_errno(eoverflow) -> 75;
get_errno(enotuniq) -> 76;
get_errno(ebadfd) -> 77;
get_errno(eremchg) -> 78;
get_errno(elibacc) -> 79;
get_errno(elibbad) -> 80;
get_errno(elibscn) -> 81;
get_errno(elibmax) -> 82;
get_errno(elibexec) -> 83;
get_errno(eilseq) -> 84;
get_errno(erestart) -> 85;
get_errno(estrpipe) -> 86;
get_errno(eusers) -> 87;
get_errno(enotsock) -> 88;
get_errno(edestaddrreq) -> 89;
get_errno(emsgsize) -> 90;
get_errno(eprototype) -> 91;
get_errno(enoprotoopt) -> 92;
get_errno(eprotonosupport) -> 93;
get_errno(esocktnosupport) -> 94;
get_errno(enotsup) -> 95;
get_errno(eopnotsupp) -> 95;
get_errno(epfnosupport) -> 96;
get_errno(eafnosupport) -> 97;
get_errno(eaddrinuse) -> 98;
get_errno(eaddrnotavail) -> 99;
get_errno(enetdown) -> 100;
get_errno(enetunreach) -> 101;
get_errno(enetreset) -> 102;
get_errno(econnaborted) -> 103;
get_errno(econnreset) -> 104;
get_errno(enobufs) -> 105;
get_errno(eisconn) -> 106;
get_errno(enotconn) -> 107;
get_errno(eshutdown) -> 108;
get_errno(etoomanyrefs) -> 109;
get_errno(etimedout) -> 110;
get_errno(econnrefused) -> 111;
get_errno(ehostdown) -> 112;
get_errno(ehostunreach) -> 113;
get_errno(ealready) -> 114;
get_errno(einprogress) -> 115;
get_errno(estale) -> 116;
get_errno(euclean) -> 117;
get_errno(enotnam) -> 118;
get_errno(enavail) -> 119;
get_errno(eisnam) -> 120;
get_errno(eremoteio) -> 121;
get_errno(edquot) -> 122;
get_errno(enomedium) -> 123;
get_errno(emediumtype) -> 124;
get_errno(ecanceled) -> 125;
get_errno(enokey) -> 126;
get_errno(ekeyexpired) -> 127;
get_errno(ekeyrevoked) -> 128;
get_errno(ekeyrejected) -> 129;
get_errno(eownerdead) -> 130;
get_errno(enotrecoverable) -> 131;
get_errno(erfkill) -> 132;
get_errno(ehwpoison) -> 133;
get_errno(_) -> 0.
