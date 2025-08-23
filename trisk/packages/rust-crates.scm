;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (trisk packages rust-crates)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages rust-sources)
  #:export (lookup-cargo-inputs))

;;;
;;; This file is managed by ‘guix import’.  Do NOT add definitions manually.
;;;

;;;
;;; Rust libraries fetched from crates.io and non-workspace development
;;; snapshots.
;;;

(define qqqq-separator 'begin-of-crates)

(define rust-aho-corasick-1.1.2
  (crate-source "aho-corasick" "1.1.2"
                "1w510wnixvlgimkx1zjbvlxh6xps2vjgfqgwf5a6adlbjp5rv5mj"))

(define rust-anstream-0.6.5
  (crate-source "anstream" "0.6.5"
                "1dm1mdbs1x6y3m3pz0qlamgiskb50i4q859676kx0pz8r8pajr6n"))

(define rust-anstyle-1.0.4
  (crate-source "anstyle" "1.0.4"
                "11yxw02b6parn29s757z96rgiqbn8qy0fk9a3p3bhczm85dhfybh"))

(define rust-anstyle-parse-0.2.3
  (crate-source "anstyle-parse" "0.2.3"
                "134jhzrz89labrdwxxnjxqjdg06qvaflj1wkfnmyapwyldfwcnn7"))

(define rust-anstyle-query-1.0.2
  (crate-source "anstyle-query" "1.0.2"
                "0j3na4b1nma39g4x7cwvj009awxckjf3z2vkwhldgka44hqj72g2"))

(define rust-anstyle-wincon-3.0.2
  (crate-source "anstyle-wincon" "3.0.2"
                "19v0fv400bmp4niqpzxnhg83vz12mmqv7l2l8vi80qcdxj0lpm8w"))

(define rust-anyhow-1.0.78
  (crate-source "anyhow" "1.0.78"
                "0l9h7k6dcq5p48zp4d777c2xsr82cb5k3gfgjvf5dc9z7q5871ya"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.4.1
  (crate-source "bitflags" "2.4.1"
                "01ryy3kd671b0ll4bhdvhsz67vwz1lz53fz504injrd7wpv64xrj"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-clap-4.4.13
  (crate-source "clap" "4.4.13"
                "0hmnmbp4v534q8aimi43qy032q21lrnyzhgdkry7zk6awj2wigaj"))

(define rust-clap-builder-4.4.12
  (crate-source "clap_builder" "4.4.12"
                "1sbrbbif0cv474ja0kcmbvc4myajlk5jymlifyzc7bkrx7jbazzv"))

(define rust-clap-derive-4.4.7
  (crate-source "clap_derive" "4.4.7"
                "0hk4hcxl56qwqsf4hmf7c0gr19r9fbxk0ah2bgkr36pmmaph966g"))

(define rust-clap-lex-0.6.0
  (crate-source "clap_lex" "0.6.0"
                "1l8bragdvim7mva9flvd159dskn2bdkpl0jqrr41wnjfn8pcfbvh"))

(define rust-clap-verbosity-flag-2.1.1
  (crate-source "clap-verbosity-flag" "2.1.1"
                "0rwp3aacz3s4zlqrkiwhx83fvy66n1scfdvgz8sars6lbdgfk41w"))

(define rust-colorchoice-1.0.0
  (crate-source "colorchoice" "1.0.0"
                "1ix7w85kwvyybwi2jdkl3yva2r2bvdcc3ka2grjfzfgrapqimgxc"))

(define rust-ctor-0.1.26
  (crate-source "ctor" "0.1.26"
                "15m0wqhv12p25xkxz5dxvg23r7a6bkh7p8zi1cdhgswjhdl028vd"))

(define rust-darling-0.10.2
  (crate-source "darling" "0.10.2"
                "0n7qsp6854wm3y1q1lvylhv15zvc87ibbac1nyfmcdbyv1snww0d"))

(define rust-darling-core-0.10.2
  (crate-source "darling_core" "0.10.2"
                "16sija1jv0l754x4aa6b6fy01d1kf8m0r4id3flqipm45np61jgh"))

(define rust-darling-macro-0.10.2
  (crate-source "darling_macro" "0.10.2"
                "0wlv31cxkrjijz5gv13hvk55c9lmd781aj12c8n84sa9mksa5dfr"))

(define rust-either-1.11.0
  (crate-source "either" "1.11.0"
                "18l0cwyw18syl8b52syv6balql8mnwfyhihjqqllx5pms93iqz54"))

(define rust-emacs-0.18.0
  (crate-source "emacs" "0.18.0"
                "0r860i73b2680i2fhdl2l1wwvvmf2zksncpckgkksdcx310ak5v7"))

(define rust-emacs-macros-0.17.0
  (crate-source "emacs-macros" "0.17.0"
                "0qg1dcn5acbirq617qq2fgg9adswif2dnr292s3qnq62wzgnyrb9"))

(define rust-emacs-module-0.18.0
  (crate-source "emacs_module" "0.18.0"
                "1ypjyyv2ca3vza4sia91ckxamgfk63yd8frkvg3d4ph4fk4pn1mk"))

(define rust-env-logger-0.10.1
  (crate-source "env_logger" "0.10.1"
                "1kmy9xmfjaqfvd4wkxr1f7d16ld3h9b487vqs2q9r0s8f3kg7cwm"))

(define rust-errno-0.3.8
  (crate-source "errno" "0.3.8"
                "0ia28ylfsp36i27g1qih875cyyy4by2grf80ki8vhgh6vinf8n52"))

(define rust-fastrand-2.0.1
  (crate-source "fastrand" "2.0.1"
                "19flpv5zbzpf0rk4x77z4zf25in0brg8l7m304d3yrf47qvwxjr5"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-heck-0.4.1
  (crate-source "heck" "0.4.1"
                "1a7mqsnycv5z4z5vnv1k34548jzmc0ajic7c1j8jsaspnhw5ql4m"))

(define rust-hermit-abi-0.3.3
  (crate-source "hermit-abi" "0.3.3"
                "1dyc8qsjh876n74a3rcz8h43s27nj1sypdhsn2ms61bd3b47wzyp"))

(define rust-home-0.5.9
  (crate-source "home" "0.5.9"
                "19grxyg35rqfd802pcc9ys1q3lafzlcjcv2pl2s5q8xpyr5kblg3"))

(define rust-humantime-2.1.0
  (crate-source "humantime" "2.1.0"
                "1r55pfkkf5v0ji1x6izrjwdq9v6sc7bv99xj6srywcar37xmnfls"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-is-terminal-0.4.10
  (crate-source "is-terminal" "0.4.10"
                "0m9la3f7cs77y85nkbcjsxkb7k861fc6bdhahyfidgh7gljh1b8b"))

(define rust-itoa-1.0.10
  (crate-source "itoa" "1.0.10"
                "0k7xjfki7mnv6yzjrbnbnjllg86acmbnk4izz2jmm1hx2wd6v95i"))

(define rust-lazy-static-1.4.0
  (crate-source "lazy_static" "1.4.0"
                "0in6ikhw8mgl33wjv6q6xfrb5b9jr16q8ygjy803fay4zcisvaz2"))

(define rust-libc-0.2.153
  (crate-source "libc" "0.2.153"
                "1gg7m1ils5dms5miq9fyllrcp0jxnbpgkx71chd2i0lafa8qy6cw"))

(define rust-linux-raw-sys-0.4.12
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.12"
                "0mhlla3gk1jgn6mrq9s255rvvq8a1w3yk2vpjiwsd6hmmy1imkf4"))

(define rust-log-0.4.20
  (crate-source "log" "0.4.20"
                "13rf7wphnwd61vazpxr7fiycin6cb1g8fmvgqg18i464p0y1drmm"))

(define rust-memchr-2.7.1
  (crate-source "memchr" "2.7.1"
                "0jf1kicqa4vs9lyzj4v4y1p90q0dh87hvhsdd5xvhnp527sw8gaj"))

(define rust-once-cell-1.19.0
  (crate-source "once_cell" "1.19.0"
                "14kvw7px5z96dk4dwdm1r9cqhhy2cyj1l5n5b29mynbb8yr15nrz"))

(define rust-proc-macro2-1.0.72
  (crate-source "proc-macro2" "1.0.72"
                "1i99vh7bqgyrcxj77awzmrg5p38a754ir8nj3bn7hr6g2s1k34x2"))

(define rust-quote-1.0.33
  (crate-source "quote" "1.0.33"
                "1biw54hbbr12wdwjac55z1m2x2rylciw83qnjn564a3096jgqrsj"))

(define rust-redox-syscall-0.4.1
  (crate-source "redox_syscall" "0.4.1"
                "1aiifyz5dnybfvkk4cdab9p2kmphag1yad6iknc7aszlxxldf8j7"))

(define rust-regex-1.10.2
  (crate-source "regex" "1.10.2"
                "0hxkd814n4irind8im5c9am221ri6bprx49nc7yxv02ykhd9a2rq"))

(define rust-regex-automata-0.4.3
  (crate-source "regex-automata" "0.4.3"
                "0gs8q9yhd3kcg4pr00ag4viqxnh5l7jpyb9fsfr8hzh451w4r02z"))

(define rust-regex-syntax-0.8.2
  (crate-source "regex-syntax" "0.8.2"
                "17rd2s8xbiyf6lb4aj2nfi44zqlj98g2ays8zzj2vfs743k79360"))

(define rust-rustc-version-0.2.3
  (crate-source "rustc_version" "0.2.3"
                "02h3x57lcr8l2pm0a645s9whdh33pn5cnrwvn5cb57vcrc53x3hk"))

(define rust-rustix-0.38.32
  (crate-source "rustix" "0.38.32"
                "12fvzwnsb13svnqzsf01maz44dib8kmgp2w8cxp7f8azwrhliq35"))

(define rust-ryu-1.0.16
  (crate-source "ryu" "1.0.16"
                "0k7b90xr48ag5bzmfjp82rljasw2fx28xr3bg1lrpx7b5sljm3gr"))

(define rust-semver-0.9.0
  (crate-source "semver" "0.9.0"
                "00q4lkcj0rrgbhviv9sd4p6qmdsipkwkbra7rh11jrhq5kpvjzhx"))

(define rust-semver-parser-0.7.0
  (crate-source "semver-parser" "0.7.0"
                "18vhypw6zgccnrlm5ps1pwa0khz7ry927iznpr88b87cagr1v2iq"))

(define rust-serde-1.0.193
  (crate-source "serde" "1.0.193"
                "129b0j67594f8qg5cbyi3nyk31y97wrqihi026mba34dwrsrkp95"))

(define rust-serde-derive-1.0.193
  (crate-source "serde_derive" "1.0.193"
                "1lwlx2k7wxr1v160kpyqjfabs37gm1yxqg65383rnyrm06jnqms3"))

(define rust-serde-json-1.0.108
  (crate-source "serde_json" "1.0.108"
                "0ssj59s7lpzqh1m50kfzlnrip0p0jg9lmhn4098i33a0mhz7w71x"))

(define rust-smallvec-1.11.2
  (crate-source "smallvec" "1.11.2"
                "0w79x38f7c0np7hqfmzrif9zmn0avjvvm31b166zdk9d1aad1k2d"))

(define rust-strsim-0.10.0
  (crate-source "strsim" "0.10.0"
                "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))

(define rust-strsim-0.9.3
  (crate-source "strsim" "0.9.3"
                "0k497pv882qn3q977ckznm13vxx927g8s1swvcv68j3c1pccwik4"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.43
  (crate-source "syn" "2.0.43"
                "0lxdd5j4l904jlf9s7jahazyys10p07w2nry3x73cmfkyfsryrgf"))

(define rust-tempfile-3.9.0
  (crate-source "tempfile" "3.9.0"
                "1ypkl7rvv57n16q28psxpb61rnyhmfaif12ascdnsyljm90l3kh1"))

(define rust-termcolor-1.4.0
  (crate-source "termcolor" "1.4.0"
                "0jfllflbxxffghlq6gx4csv0bv0qv77943dcx01h9zssy39w66zz"))

(define rust-thiserror-1.0.53
  (crate-source "thiserror" "1.0.53"
                "02fczinyqdp28gl6i7zsag577qiarw9bpp8kannhib9vfq25kkdj"))

(define rust-thiserror-impl-1.0.53
  (crate-source "thiserror-impl" "1.0.53"
                "06cywbwmsfs4l6x8wcrjhvb813jc4cj6zbiqdz6yl2nf9j14mkrx"))

(define rust-unicode-ident-1.0.12
  (crate-source "unicode-ident" "1.0.12"
                "0jzf1znfpb2gx8nr8mvmyqs1crnv79l57nxnbiszc7xf7ynbjm1k"))

(define rust-utf8parse-0.2.1
  (crate-source "utf8parse" "0.2.1"
                "02ip1a0az0qmc2786vxk2nqwsgcwf17d3a38fkf0q7hrmwh9c6vi"))

(define rust-which-6.0.1
  (crate-source "which" "6.0.1"
                "1mz0vijj9qvsmfqkjqw3wf8zqn19p2x0gg7gzfnhaa1bibsy84c2"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-util-0.1.6
  (crate-source "winapi-util" "0.1.6"
                "15i5lm39wd44004i9d5qspry2cynkrpvwzghr6s2c3dsk28nz7pj"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-windows-aarch64-gnullvm-0.52.0
  (crate-source "windows_aarch64_gnullvm" "0.52.0"
                "1shmn1kbdc0bpphcxz0vlph96bxz0h1jlmh93s9agf2dbpin8xyb"))

(define rust-windows-aarch64-msvc-0.52.0
  (crate-source "windows_aarch64_msvc" "0.52.0"
                "1vvmy1ypvzdvxn9yf0b8ygfl85gl2gpcyvsvqppsmlpisil07amv"))

(define rust-windows-i686-gnu-0.52.0
  (crate-source "windows_i686_gnu" "0.52.0"
                "04zkglz4p3pjsns5gbz85v4s5aw102raz4spj4b0lmm33z5kg1m2"))

(define rust-windows-i686-msvc-0.52.0
  (crate-source "windows_i686_msvc" "0.52.0"
                "16kvmbvx0vr0zbgnaz6nsks9ycvfh5xp05bjrhq65kj623iyirgz"))

(define rust-windows-sys-0.52.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-targets-0.52.0
  (crate-source "windows-targets" "0.52.0"
                "1kg7a27ynzw8zz3krdgy6w5gbqcji27j1sz4p7xk2j5j8082064a"))

(define rust-windows-x86-64-gnu-0.52.0
  (crate-source "windows_x86_64_gnu" "0.52.0"
                "1zdy4qn178sil5sdm63lm7f0kkcjg6gvdwmcprd2yjmwn8ns6vrx"))

(define rust-windows-x86-64-gnullvm-0.52.0
  (crate-source "windows_x86_64_gnullvm" "0.52.0"
                "17lllq4l2k1lqgcnw1cccphxp9vs7inq99kjlm2lfl9zklg7wr8s"))

(define rust-windows-x86-64-msvc-0.52.0
  (crate-source "windows_x86_64_msvc" "0.52.0"
                "012wfq37f18c09ij5m6rniw7xxn5fcvrxbqd0wd8vgnl3hfn9yfz"))

(define rust-winsafe-0.0.19
  (crate-source "winsafe" "0.0.19"
                "0169xy9mjma8dys4m8v4x0xhw2gkbhv2v1wsbvcjl9bhnxxd2dfi"))

(define ssss-separator 'end-of-crates)


;;;
;;; Cargo inputs.
;;;

(define-cargo-inputs lookup-cargo-inputs
                     (emacs-lsp-booster =>
                                        (list rust-aho-corasick-1.1.2
                                         rust-anstream-0.6.5
                                         rust-anstyle-1.0.4
                                         rust-anstyle-parse-0.2.3
                                         rust-anstyle-query-1.0.2
                                         rust-anstyle-wincon-3.0.2
                                         rust-anyhow-1.0.78
                                         rust-bitflags-1.3.2
                                         rust-bitflags-2.4.1
                                         rust-cfg-if-1.0.0
                                         rust-clap-4.4.13
                                         rust-clap-verbosity-flag-2.1.1
                                         rust-clap-builder-4.4.12
                                         rust-clap-derive-4.4.7
                                         rust-clap-lex-0.6.0
                                         rust-colorchoice-1.0.0
                                         rust-ctor-0.1.26
                                         rust-darling-0.10.2
                                         rust-darling-core-0.10.2
                                         rust-darling-macro-0.10.2
                                         rust-either-1.11.0
                                         rust-emacs-0.18.0
                                         rust-emacs-macros-0.17.0
                                         rust-emacs-module-0.18.0
                                         rust-env-logger-0.10.1
                                         rust-errno-0.3.8
                                         rust-fastrand-2.0.1
                                         rust-fnv-1.0.7
                                         rust-heck-0.4.1
                                         rust-hermit-abi-0.3.3
                                         rust-home-0.5.9
                                         rust-humantime-2.1.0
                                         rust-ident-case-1.0.1
                                         rust-is-terminal-0.4.10
                                         rust-itoa-1.0.10
                                         rust-lazy-static-1.4.0
                                         rust-libc-0.2.153
                                         rust-linux-raw-sys-0.4.12
                                         rust-log-0.4.20
                                         rust-memchr-2.7.1
                                         rust-once-cell-1.19.0
                                         rust-proc-macro2-1.0.72
                                         rust-quote-1.0.33
                                         rust-redox-syscall-0.4.1
                                         rust-regex-1.10.2
                                         rust-regex-automata-0.4.3
                                         rust-regex-syntax-0.8.2
                                         rust-rustc-version-0.2.3
                                         rust-rustix-0.38.32
                                         rust-ryu-1.0.16
                                         rust-semver-0.9.0
                                         rust-semver-parser-0.7.0
                                         rust-serde-1.0.193
                                         rust-serde-derive-1.0.193
                                         rust-serde-json-1.0.108
                                         rust-smallvec-1.11.2
                                         rust-strsim-0.9.3
                                         rust-strsim-0.10.0
                                         rust-syn-1.0.109
                                         rust-syn-2.0.43
                                         rust-tempfile-3.9.0
                                         rust-termcolor-1.4.0
                                         rust-thiserror-1.0.53
                                         rust-thiserror-impl-1.0.53
                                         rust-unicode-ident-1.0.12
                                         rust-utf8parse-0.2.1
                                         rust-which-6.0.1
                                         rust-winapi-0.3.9
                                         rust-winapi-i686-pc-windows-gnu-0.4.0
                                         rust-winapi-util-0.1.6
                                         rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                         rust-windows-sys-0.52.0
                                         rust-windows-targets-0.52.0
                                         rust-windows-aarch64-gnullvm-0.52.0
                                         rust-windows-aarch64-msvc-0.52.0
                                         rust-windows-i686-gnu-0.52.0
                                         rust-windows-i686-msvc-0.52.0
                                         rust-windows-x86-64-gnu-0.52.0
                                         rust-windows-x86-64-gnullvm-0.52.0
                                         rust-windows-x86-64-msvc-0.52.0
                                         rust-winsafe-0.0.19)))
