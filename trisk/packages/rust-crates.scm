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

(define rust-ab-glyph-0.2.32
  (crate-source "ab_glyph" "0.2.32"
                "1hkc7y8yjd261d5cm9771dawnwc26rgdlniv3jysb3n3f9s4bh01"))

(define rust-ab-glyph-rasterizer-0.1.10
  (crate-source "ab_glyph_rasterizer" "0.1.10"
                "065n6bj7kqk6f12336lm87fqmvf4lxg7rkg2j56nix228jmgnvrn"))

(define rust-addr2line-0.24.2
  (crate-source "addr2line" "0.24.2"
                "1hd1i57zxgz08j6h5qrhsnm2fi0bcqvsh389fw400xm3arz2ggnz"))

(define rust-adler2-2.0.0
  (crate-source "adler2" "2.0.0"
                "09r6drylvgy8vv8k20lnbvwq8gp09h7smfn6h1rxsy15pgh629si"))

(define rust-adler2-2.0.1
  (crate-source "adler2" "2.0.1"
                "1ymy18s9hs7ya1pjc9864l30wk8p2qfqdi7mhhcc5nfakxbij09j"))

(define rust-aes-0.8.4
  (crate-source "aes" "0.8.4"
                "1853796anlwp4kqim0s6wm1srl4ib621nm0cl2h3c8klsjkgfsdi"))

(define rust-ahash-0.8.12
  (crate-source "ahash" "0.8.12"
                "0xbsp9rlm5ki017c0w6ay8kjwinwm8knjncci95mii30rmwz25as"))

(define rust-aho-corasick-1.1.2
  (crate-source "aho-corasick" "1.1.2"
                "1w510wnixvlgimkx1zjbvlxh6xps2vjgfqgwf5a6adlbjp5rv5mj"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-aho-corasick-1.1.4
  (crate-source "aho-corasick" "1.1.4"
                "00a32wb2h07im3skkikc495jvncf62jl6s96vwc7bhi70h9imlyx"))

(define rust-android-activity-0.6.0
  (crate-source "android-activity" "0.6.0"
                "0inh88x8x2fh62jg739s9hwyvdh8i920qf0qw7bhr802j9c7hsgg"))

(define rust-android-build-0.1.3
  (crate-source "android-build" "0.1.3"
                "1zdqn0h2kya3f05g55vg8x53ixh7khrmd5r3rw44cl2x2xj4rb4c"))

(define rust-android-properties-0.2.2
  (crate-source "android-properties" "0.2.2"
                "016slvg269c0y120p9qd8vdfqa2jbw4j0g18gfw6p3ain44v4zpw"))

(define rust-android-system-properties-0.1.5
  (crate-source "android_system_properties" "0.1.5"
                "04b3wrz12837j7mdczqd95b732gw5q7q66cv4yn4646lvccp57l1"))

(define rust-anstream-0.6.18
  (crate-source "anstream" "0.6.18"
                "16sjk4x3ns2c3ya1x28a44kh6p47c7vhk27251i015hik1lm7k4a"))

(define rust-anstream-0.6.21
  (crate-source "anstream" "0.6.21"
                "0jjgixms4qjj58dzr846h2s29p8w7ynwr9b9x6246m1pwy0v5ma3"))

(define rust-anstream-0.6.5
  (crate-source "anstream" "0.6.5"
                "1dm1mdbs1x6y3m3pz0qlamgiskb50i4q859676kx0pz8r8pajr6n"))

(define rust-anstyle-1.0.10
  (crate-source "anstyle" "1.0.10"
                "1yai2vppmd7zlvlrp9grwll60knrmscalf8l2qpfz8b7y5lkpk2m"))

(define rust-anstyle-1.0.13
  (crate-source "anstyle" "1.0.13"
                "0y2ynjqajpny6q0amvfzzgw0gfw3l47z85km4gvx87vg02lcr4ji"))

(define rust-anstyle-1.0.4
  (crate-source "anstyle" "1.0.4"
                "11yxw02b6parn29s757z96rgiqbn8qy0fk9a3p3bhczm85dhfybh"))

(define rust-anstyle-parse-0.2.3
  (crate-source "anstyle-parse" "0.2.3"
                "134jhzrz89labrdwxxnjxqjdg06qvaflj1wkfnmyapwyldfwcnn7"))

(define rust-anstyle-parse-0.2.6
  (crate-source "anstyle-parse" "0.2.6"
                "1acqayy22fwzsrvr6n0lz6a4zvjjcvgr5sm941m7m0b2fr81cb9v"))

(define rust-anstyle-parse-0.2.7
  (crate-source "anstyle-parse" "0.2.7"
                "1hhmkkfr95d462b3zf6yl2vfzdqfy5726ya572wwg8ha9y148xjf"))

(define rust-anstyle-query-1.0.2
  (crate-source "anstyle-query" "1.0.2"
                "0j3na4b1nma39g4x7cwvj009awxckjf3z2vkwhldgka44hqj72g2"))

(define rust-anstyle-query-1.1.2
  (crate-source "anstyle-query" "1.1.2"
                "036nm3lkyk43xbps1yql3583fp4hg3b1600is7mcyxs1gzrpm53r"))

(define rust-anstyle-query-1.1.5
  (crate-source "anstyle-query" "1.1.5"
                "1p6shfpnbghs6jsa0vnqd8bb8gd7pjd0jr7w0j8jikakzmr8zi20"))

(define rust-anstyle-wincon-3.0.11
  (crate-source "anstyle-wincon" "3.0.11"
                "0zblannm70sk3xny337mz7c6d8q8i24vhbqi42ld8v7q1wjnl7i9"))

(define rust-anstyle-wincon-3.0.2
  (crate-source "anstyle-wincon" "3.0.2"
                "19v0fv400bmp4niqpzxnhg83vz12mmqv7l2l8vi80qcdxj0lpm8w"))

(define rust-anstyle-wincon-3.0.6
  (crate-source "anstyle-wincon" "3.0.6"
                "099ir0w3lbpsp1nxdzbf4anq98ww8ykyc9pd1g03xgkj1v7dn291"))

(define rust-anyhow-1.0.101
  (crate-source "anyhow" "1.0.101"
                "1skmg90fnjnlgs3vl7bksw7036d3rqwqj20n2fxd2ppg67p0y3jz"))

(define rust-anyhow-1.0.78
  (crate-source "anyhow" "1.0.78"
                "0l9h7k6dcq5p48zp4d777c2xsr82cb5k3gfgjvf5dc9z7q5871ya"))

(define rust-arbitrary-1.4.2
  (crate-source "arbitrary" "1.4.2"
                "1wcbi4x7i3lzcrkjda4810nqv03lpmvfhb0a85xrq1mbqjikdl63"))

(define rust-arrayref-0.3.9
  (crate-source "arrayref" "0.3.9"
                "1jzyp0nvp10dmahaq9a2rnxqdd5wxgbvp8xaibps3zai8c9fi8kn"))

(define rust-arrayvec-0.7.6
  (crate-source "arrayvec" "0.7.6"
                "0l1fz4ccgv6pm609rif37sl5nv5k6lbzi7kkppgzqzh1vwix20kw"))

(define rust-as-raw-xcb-connection-1.0.1
  (crate-source "as-raw-xcb-connection" "1.0.1"
                "0sqgpz2ymv5yx76r5j2npjq2x5qvvqnw0vrs35cyv30p3pfp2m8p"))

(define rust-ash-0.38.0+1.3.281
  (crate-source "ash" "0.38.0+1.3.281"
                "0vx4yf689v1rc680jvy8bnysx5sgd8f33wnp2vqaizh0v0v4kd0b"))

(define rust-async-broadcast-0.7.1
  (crate-source "async-broadcast" "0.7.1"
                "0zia7z1y31awmxma9c89zmvkbj7mdkf7highkmz5z3pa4lp0xk90"))

(define rust-async-broadcast-0.7.2
  (crate-source "async-broadcast" "0.7.2"
                "0ckmqcwyqwbl2cijk1y4r0vy60i89gqc86ijrxzz5f2m4yjqfnj3"))

(define rust-async-channel-2.3.1
  (crate-source "async-channel" "2.3.1"
                "0skvwxj6ysfc6d7bhczz9a2550260g62bm5gl0nmjxxyn007id49"))

(define rust-async-channel-2.5.0
  (crate-source "async-channel" "2.5.0"
                "1ljq24ig8lgs2555myrrjighycpx2mbjgrm3q7lpa6rdsmnxjklj"))

(define rust-async-executor-1.13.1
  (crate-source "async-executor" "1.13.1"
                "1v6w1dbvsmw6cs4dk4lxj5dvrikc6xi479wikwaab2qy3h09mjih"))

(define rust-async-executor-1.13.3
  (crate-source "async-executor" "1.13.3"
                "1f3za9v8wkqzv6rz69g0qzvdcmghwbixijwzldwjm9w3zph00z29"))

(define rust-async-fs-2.1.2
  (crate-source "async-fs" "2.1.2"
                "0jp0p7lg9zqy2djgdmivbzx0yqmfn9sm2s9dkhaws3zlharhkkgb"))

(define rust-async-io-2.4.0
  (crate-source "async-io" "2.4.0"
                "0n8h0vy53n4vdkq529scqnkzm9vcl3r73za9nj81s2nfrhiv78j3"))

(define rust-async-io-2.6.0
  (crate-source "async-io" "2.6.0"
                "1z16s18bm4jxlmp6rif38mvn55442yd3wjvdfhvx4hkgxf7qlss5"))

(define rust-async-lock-3.4.0
  (crate-source "async-lock" "3.4.0"
                "060vh45i809wcqyxzs5g69nqiqah7ydz0hpkcjys9258vqn4fvpz"))

(define rust-async-lock-3.4.2
  (crate-source "async-lock" "3.4.2"
                "04c3xrrdrfrvh9v0ajxrangpy38qi76qq268zslphnxxjqjpy3r9"))

(define rust-async-process-2.3.0
  (crate-source "async-process" "2.3.0"
                "1fr6cpqdw7hrmzns1983lgx86cg8vyz7nlrn0h0125iqq8fmy9b3"))

(define rust-async-process-2.5.0
  (crate-source "async-process" "2.5.0"
                "0xfswxmng6835hjlfhv7k0jrfp7czqxpfj6y2s5dsp05q0g94l7w"))

(define rust-async-recursion-1.1.1
  (crate-source "async-recursion" "1.1.1"
                "04ac4zh8qz2xjc79lmfi4jlqj5f92xjvfaqvbzwkizyqd4pl4hrv"))

(define rust-async-signal-0.2.10
  (crate-source "async-signal" "0.2.10"
                "1wxrq3871l00mil43nmh0akvwjjjnv0bn7n2pzwbvh00k0s00zk3"))

(define rust-async-signal-0.2.13
  (crate-source "async-signal" "0.2.13"
                "0k66mpb3xp86hj4vxs7w40v7qz2jfbblrm9ddc5mglwwynxp1h23"))

(define rust-async-task-4.7.1
  (crate-source "async-task" "4.7.1"
                "1pp3avr4ri2nbh7s6y9ws0397nkx1zymmcr14sq761ljarh3axcb"))

(define rust-async-trait-0.1.83
  (crate-source "async-trait" "0.1.83"
                "1p8q8gm4fv2fdka8hwy2w3f8df7p5inixqi7rlmbnky3wmysw73j"))

(define rust-async-trait-0.1.89
  (crate-source "async-trait" "0.1.89"
                "1fsxxmz3rzx1prn1h3rs7kyjhkap60i7xvi0ldapkvbb14nssdch"))

(define rust-atomic-waker-1.1.2
  (crate-source "atomic-waker" "1.1.2"
                "1h5av1lw56m0jf0fd3bchxq8a30xv0b4wv8s4zkp4s0i7mfvs18m"))

(define rust-autocfg-1.4.0
  (crate-source "autocfg" "1.4.0"
                "09lz3by90d2hphbq56znag9v87gfpd9gb8nr82hll8z6x2nhprdc"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-backtrace-0.3.74
  (crate-source "backtrace" "0.3.74"
                "06pfif7nwx66qf2zaanc2fcq7m64i91ki9imw9xd3bnz5hrwp0ld"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

(define rust-bit-set-0.8.0
  (crate-source "bit-set" "0.8.0"
                "18riaa10s6n59n39vix0cr7l2dgwdhcpbcm97x1xbyfp1q47x008"))

(define rust-bit-vec-0.8.0
  (crate-source "bit-vec" "0.8.0"
                "1xxa1s2cj291r7k1whbxq840jxvmdsq9xgh7bvrxl46m80fllxjy"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.10.0
  (crate-source "bitflags" "2.10.0"
                "1lqxwc3625lcjrjm5vygban9v8a6dlxisp1aqylibiaw52si4bl1"))

(define rust-bitflags-2.4.1
  (crate-source "bitflags" "2.4.1"
                "01ryy3kd671b0ll4bhdvhsz67vwz1lz53fz504injrd7wpv64xrj"))

(define rust-bitflags-2.6.0
  (crate-source "bitflags" "2.6.0"
                "1pkidwzn3hnxlsl8zizh0bncgbjnw7c41cx7bby26ncbzmiznj5h"))

(define rust-bitflags-2.9.4
  (crate-source "bitflags" "2.9.4"
                "157kkcv8s7vk6d17dar1pa5cqcz4c8pdrn16wm1ld7jnr86d2q92"))

(define rust-bitvec-1.0.1
  (crate-source "bitvec" "1.0.1"
                "173ydyj2q5vwj88k6xgjnfsshs4x9wbvjjv7sm0h36r34hn87hhv"))

(define rust-block-0.1.6
  (crate-source "block" "0.1.6"
                "16k9jgll25pzsq14f244q22cdv0zb4bqacldg3kx6h89d7piz30d"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-block2-0.5.1
  (crate-source "block2" "0.5.1"
                "0pyiha5his2grzqr3mynmq244laql2j20992i59asp0gy7mjw4rc"))

(define rust-block2-0.6.2
  (crate-source "block2" "0.6.2"
                "1xcfllzx6c3jc554nmb5qy6xmlkl6l6j5ib4wd11800n0n3rvsyd"))

(define rust-blocking-1.6.1
  (crate-source "blocking" "1.6.1"
                "1si99l8zp7c4zq87y35ayjgc5c9b60jb8h0k14zfcs679z2l2gvh"))

(define rust-blocking-1.6.2
  (crate-source "blocking" "1.6.2"
                "08bz3f9agqlp3102snkvsll6wc9ag7x5m1xy45ak2rv9pq18sgz8"))

(define rust-bumpalo-3.19.0
  (crate-source "bumpalo" "3.19.0"
                "0hsdndvcpqbjb85ghrhska2qxvp9i75q2vb70hma9fxqawdy9ia6"))

(define rust-bumpalo-3.19.1
  (crate-source "bumpalo" "3.19.1"
                "044555i277xcinmqs7nnv8n5y4fqfi4l4lp1mp3i30vsidrxrnax"))

(define rust-bytecount-0.6.9
  (crate-source "bytecount" "0.6.9"
                "0pinq0n8zza8qr2lyc3yf17k963129kdbf0bwnmvdk1bpvh14n0p"))

(define rust-bytemuck-1.25.0
  (crate-source "bytemuck" "1.25.0"
                "1v1z32igg9zq49phb3fra0ax5r2inf3aw473vldnm886sx5vdvy8"))

(define rust-bytemuck-derive-1.10.2
  (crate-source "bytemuck_derive" "1.10.2"
                "1zvmjmw1sdmx9znzm4dpbb2yvz9vyim8w6gp4z256l46qqdvvazr"))

(define rust-byteorder-lite-0.1.0
  (crate-source "byteorder-lite" "0.1.0"
                "15alafmz4b9az56z6x7glcbcb6a8bfgyd109qc3bvx07zx4fj7wg"))

(define rust-bytes-1.11.0
  (crate-source "bytes" "1.11.0"
                "1cww1ybcvisyj8pbzl4m36bni2jaz0narhczp1348gqbvkxh8lmk"))

(define rust-bytes-1.11.1
  (crate-source "bytes" "1.11.1"
                "0czwlhbq8z29wq0ia87yass2mzy1y0jcasjb8ghriiybnwrqfx0y"))

(define rust-bytes-1.8.0
  (crate-source "bytes" "1.8.0"
                "1nnhpb7jlpj393qnjr1n9n6sgpl3w5ymrwl3pnjmrriam861bh4s"))

(define rust-bzip2-0.6.1
  (crate-source "bzip2" "0.6.1"
                "0v1lgjxy944fdvsl97wmqs7f288crv7xddalk6y82jpk4jn3z9gk"))

(define rust-calloop-0.13.0
  (crate-source "calloop" "0.13.0"
                "1v5zgidnhsyml403rzr7vm99f8q6r5bxq5gxyiqkr8lcapwa57dr"))

(define rust-calloop-0.14.3
  (crate-source "calloop" "0.14.3"
                "17ih3c840cqksv9ms7i2ynnkiabpvqvpxakbr3922imxd09nx7yb"))

(define rust-calloop-wayland-source-0.3.0
  (crate-source "calloop-wayland-source" "0.3.0"
                "086x5mq16prrcwd9k6bw9an0sp8bj9l5daz4ziz5z4snf2c6m9lm"))

(define rust-calloop-wayland-source-0.4.1
  (crate-source "calloop-wayland-source" "0.4.1"
                "1yi1c23naqhd8m94q3v366s4cak8l50zy7ldrkqfn0hajkqgr3hk"))

(define rust-cc-1.2.35
  (crate-source "cc" "1.2.35"
                "18vfhd6njr0lhfvfvxchj3bay4fw6hcpyy4130sl134alqj903sr"))

(define rust-cc-1.2.49
  (crate-source "cc" "1.2.49"
                "05929ra8a2q81w45f932nr4blifnxkpr8i7lmcba28bm0c4k0n4h"))

(define rust-cc-1.2.55
  (crate-source "cc" "1.2.55"
                "0adx36r84c7rscv853a71nd3d5gsb1jf438gnl4syd5fah4nmcj7"))

(define rust-cesu8-1.1.0
  (crate-source "cesu8" "1.1.0"
                "0g6q58wa7khxrxcxgnqyi9s1z2cjywwwd3hzr5c55wskhx6s0hvd"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-cfg-if-1.0.3
  (crate-source "cfg-if" "1.0.3"
                "1afg7146gbxjvkbjx7i5sdrpqp9q5akmk9004fr8rsm90jf2il9g"))

(define rust-cfg-if-1.0.4
  (crate-source "cfg-if" "1.0.4"
                "008q28ajc546z5p2hcwdnckmg0hia7rnx52fni04bwqkzyrghc4k"))

(define rust-cipher-0.4.4
  (crate-source "cipher" "0.4.4"
                "1b9x9agg67xq5nq879z66ni4l08m6m3hqcshk37d4is4ysd3ngvp"))

(define rust-clap-4.4.13
  (crate-source "clap" "4.4.13"
                "0hmnmbp4v534q8aimi43qy032q21lrnyzhgdkry7zk6awj2wigaj"))

(define rust-clap-4.5.53
  (crate-source "clap" "4.5.53"
                "1y035lyy5w2xx83q4c3jiy75928ldm1x2bi8ylslkgx12bh41qy9"))

(define rust-clap-4.5.57
  (crate-source "clap" "4.5.57"
                "06p3x91f3yq4lz80lc0z56mmz314sbizdsymcmd31f9zkr4ym6b8"))

(define rust-clap-builder-4.4.12
  (crate-source "clap_builder" "4.4.12"
                "1sbrbbif0cv474ja0kcmbvc4myajlk5jymlifyzc7bkrx7jbazzv"))

(define rust-clap-builder-4.5.53
  (crate-source "clap_builder" "4.5.53"
                "004xasw24a9vvzpiymjkm4khffpyzqwskz7ps8gr1351x89mssyp"))

(define rust-clap-builder-4.5.57
  (crate-source "clap_builder" "4.5.57"
                "0f728m14vqrvcpp8pxmaz74c5dh1gd2bh5jd6sl64nhrh2vch4kv"))

(define rust-clap-derive-4.4.7
  (crate-source "clap_derive" "4.4.7"
                "0hk4hcxl56qwqsf4hmf7c0gr19r9fbxk0ah2bgkr36pmmaph966g"))

(define rust-clap-derive-4.5.49
  (crate-source "clap_derive" "4.5.49"
                "0wbngw649138v3jwx8pm5x9sq0qsml3sh0sfzyrdxcpamy3m82ra"))

(define rust-clap-derive-4.5.55
  (crate-source "clap_derive" "4.5.55"
                "1r949xis3jmhzh387smd70vc8a3b9734ck3g5ahg59a63bd969x9"))

(define rust-clap-lex-0.6.0
  (crate-source "clap_lex" "0.6.0"
                "1l8bragdvim7mva9flvd159dskn2bdkpl0jqrr41wnjfn8pcfbvh"))

(define rust-clap-lex-0.7.6
  (crate-source "clap_lex" "0.7.6"
                "13cxw9m2rqvplgazgkq2awms0rgf34myc19bz6gywfngi762imx1"))

(define rust-clap-lex-0.7.7
  (crate-source "clap_lex" "0.7.7"
                "0cibsbziyzw2ywar2yh6zllsamhwkblfly565zgi56s3q064prn3"))

(define rust-clap-verbosity-flag-2.1.1
  (crate-source "clap-verbosity-flag" "2.1.1"
                "0rwp3aacz3s4zlqrkiwhx83fvy66n1scfdvgz8sars6lbdgfk41w"))

(define rust-clipboard-macos-0.1.1
  (crate-source "clipboard_macos" "0.1.1"
                "13zkbmnsl2c98452kvia7vr2ywwl123bj2q81diw78vv0jm4lzwv"))

(define rust-clipboard-wayland-0.2.2
  (crate-source "clipboard_wayland" "0.2.2"
                "1f1prf3qhww5qqbbqbf27ygq103z9r1b678cs4lpg672qimqhgq0"))

(define rust-clipboard-win-5.4.1
  (crate-source "clipboard-win" "5.4.1"
                "1m44gqy11rq1ww7jls86ppif98v6kv2wkwk8p17is86zsdq3gq5x"))

(define rust-clipboard-x11-0.4.3
  (crate-source "clipboard_x11" "0.4.3"
                "1m2szxgxk8s8j7anp73pdijdp50yvnjhakscj8wzvnpza8sf6qxx"))

(define rust-codespan-reporting-0.12.0
  (crate-source "codespan-reporting" "0.12.0"
                "108g41xqzhr8fx8hlpy5qzmqq8ylldbj37wndkaqm34yy1d2wvgy"))

(define rust-color-quant-1.1.0
  (crate-source "color_quant" "1.1.0"
                "12q1n427h2bbmmm1mnglr57jaz2dj9apk0plcxw7nwqiai7qjyrx"))

(define rust-colorchoice-1.0.0
  (crate-source "colorchoice" "1.0.0"
                "1ix7w85kwvyybwi2jdkl3yva2r2bvdcc3ka2grjfzfgrapqimgxc"))

(define rust-colorchoice-1.0.3
  (crate-source "colorchoice" "1.0.3"
                "1439m3r3jy3xqck8aa13q658visn71ki76qa93cy55wkmalwlqsv"))

(define rust-colorchoice-1.0.4
  (crate-source "colorchoice" "1.0.4"
                "0x8ymkz1xr77rcj1cfanhf416pc4v681gmkc9dzb3jqja7f62nxh"))

(define rust-combine-4.6.7
  (crate-source "combine" "4.6.7"
                "1z8rh8wp59gf8k23ar010phgs0wgf5i8cx4fg01gwcnzfn5k0nms"))

(define rust-concurrent-queue-2.5.0
  (crate-source "concurrent-queue" "2.5.0"
                "0wrr3mzq2ijdkxwndhf79k952cp4zkz35ray8hvsxl96xrx1k82c"))

(define rust-confy-2.0.0
  (crate-source "confy" "2.0.0"
                "0528xqlri0kwjlzjlyv910l1712m6igyl9qsvfxh5glwg2bw61w8"))

(define rust-constant-time-eq-0.3.1
  (crate-source "constant_time_eq" "0.3.1"
                "19nwwczii762pwlsm7bpizgjg8hkg1kqi32b2g4rglijklsbhx3w"))

(define rust-core-foundation-0.10.1
  (crate-source "core-foundation" "0.10.1"
                "1xjns6dqf36rni2x9f47b65grxwdm20kwdg9lhmzdrrkwadcv9mj"))

(define rust-core-foundation-0.9.4
  (crate-source "core-foundation" "0.9.4"
                "13zvbbj07yk3b61b8fhwfzhy35535a583irf23vlcg59j7h9bqci"))

(define rust-core-foundation-sys-0.8.7
  ;; TODO: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-core-graphics-0.23.2
  (crate-source "core-graphics" "0.23.2"
                "10dhv3gk4kmbzl14xxkrhhky4fdp8h6nzff6h0019qgr6nz84xy0"))

(define rust-core-graphics-types-0.1.3
  (crate-source "core-graphics-types" "0.1.3"
                "1bxg8nxc8fk4kxnqyanhf36wq0zrjr552c58qy6733zn2ihhwfa5"))

(define rust-core-graphics-types-0.2.0
  (crate-source "core-graphics-types" "0.2.0"
                "1sqka1rz84lr3p69i1s6lggnpnznmrw4ngc5q76w9xhky80s2i1x"))

(define rust-core-maths-0.1.1
  (crate-source "core_maths" "0.1.1"
                "0c0dv11ixxpc9bsx5xasvl98mb1dlprzcm6qq6ls3nsygw0mwx3p"))

(define rust-cosmic-text-0.15.0
  (crate-source "cosmic-text" "0.15.0"
                "0dvpwc4rb7gysrd7sir10k2hk2j2vis6xn35wg5s6mws78l54f0p"))

(define rust-cpufeatures-0.2.17
  (crate-source "cpufeatures" "0.2.17"
                "10023dnnaghhdl70xcds12fsx2b966sxbxjq5sxs49mvxqw5ivar"))

(define rust-crc-3.4.0
  (crate-source "crc" "3.4.0"
                "03dsq5qsv86m35ikg84l80d00wnkjm8q4pjxgac0vaqjrnhs5f2y"))

(define rust-crc-catalog-2.4.0
  (crate-source "crc-catalog" "2.4.0"
                "1xg7sz82w3nxp1jfn425fvn1clvbzb3zgblmxsyqpys0dckp9lqr"))

(define rust-crc32fast-1.5.0
  (crate-source "crc32fast" "1.5.0"
                "04d51liy8rbssra92p0qnwjw8i9rm9c4m3bwy19wjamz1k4w30cl"))

(define rust-crossbeam-utils-0.8.20
  (crate-source "crossbeam-utils" "0.8.20"
                "100fksq5mm1n7zj242cclkw6yf7a4a8ix3lvpfkhxvdhbda9kv12"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-crunchy-0.2.4
  (crate-source "crunchy" "0.2.4"
                "1mbp5navim2qr3x48lyvadqblcxc1dm0lqr0swrkkwy2qblvw3s6"))

(define rust-cryoglyph-0.1.0
  (crate-source "cryoglyph" "0.1.0"
                "06f5dvmsirrkrrpnyv92gdcsd7801a9n7cbgfdhx9nycvddpkg08"))

(define rust-crypto-common-0.1.7
  (crate-source "crypto-common" "0.1.7"
                "02nn2rhfy7kvdkdjl457q2z0mklcvj9h662xrq6dzhfialh2kj3q"))

(define rust-ctor-0.1.26
  (crate-source "ctor" "0.1.26"
                "15m0wqhv12p25xkxz5dxvg23r7a6bkh7p8zi1cdhgswjhdl028vd"))

(define rust-ctor-lite-0.1.1
  (crate-source "ctor-lite" "0.1.1"
                "1z8iw7a1w543zi614q09x77wzxm2xb8b2napva8bvydhmvywr7xj"))

(define rust-cursor-icon-1.2.0
  (crate-source "cursor-icon" "1.2.0"
                "0bvkw7ak1mqwcpkgd9lh7n00hcvlh87jfl7188f231nz6zfy2ypj"))

(define rust-darling-0.10.2
  (crate-source "darling" "0.10.2"
                "0n7qsp6854wm3y1q1lvylhv15zvc87ibbac1nyfmcdbyv1snww0d"))

(define rust-darling-core-0.10.2
  (crate-source "darling_core" "0.10.2"
                "16sija1jv0l754x4aa6b6fy01d1kf8m0r4id3flqipm45np61jgh"))

(define rust-darling-macro-0.10.2
  (crate-source "darling_macro" "0.10.2"
                "0wlv31cxkrjijz5gv13hvk55c9lmd781aj12c8n84sa9mksa5dfr"))

(define rust-data-url-0.3.2
  (crate-source "data-url" "0.3.2"
                "0xl30jidc8s3kh2z3nvnn1nyzhbq5b2wpiqwzj9gjdrndk50n7my"))

(define rust-deflate64-0.1.10
  (crate-source "deflate64" "0.1.10"
                "012jmx4jrxwdk5d6fbnnxih4zdq9nb0vmjzhqasjkvf5a71qzgr6"))

(define rust-deranged-0.5.5
  (crate-source "deranged" "0.5.5"
                "11z5939gv2klp1r1lgrp4w5fnlkj18jqqf0h9zxmia3vkrjwpv7c"))

(define rust-derive-arbitrary-1.4.2
  (crate-source "derive_arbitrary" "1.4.2"
                "0annkmfwfavd978vwwrxvrpykjfdnc3w6q1ln3j7kyfg5pc7nmhy"))

(define rust-diff-0.1.13
  (crate-source "diff" "0.1.13"
                "1j0nzjxci2zqx63hdcihkp0a4dkdmzxd7my4m7zk6cjyfy34j9an"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-dispatch-0.2.0
  (crate-source "dispatch" "0.2.0"
                "0fwjr9b7582ic5689zxj8lf7zl94iklhlns3yivrnv8c9fxr635x"))

(define rust-dispatch2-0.3.0
  (crate-source "dispatch2" "0.3.0"
                "1v1ak9w0s8z1g13x4mj2y5im9wmck0i2vf8f8wc9l1n6lqi9z849"))

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

(define rust-dlib-0.5.2
  (crate-source "dlib" "0.5.2"
                "04m4zzybx804394dnqs1blz241xcy480bdwf3w9p4k6c3l46031k"))

(define rust-document-features-0.2.12
  (crate-source "document-features" "0.2.12"
                "0qcgpialq3zgvjmsvar9n6v10rfbv6mk6ajl46dd4pj5hn3aif6l"))

(define rust-downcast-rs-1.2.1
  (crate-source "downcast-rs" "1.2.1"
                "1lmrq383d1yszp7mg5i7i56b17x2lnn3kb91jwsq0zykvg2jbcvm"))

(define rust-dpi-0.1.2
  (crate-source "dpi" "0.1.2"
                "0xhsvzgjvdch2fwmfc9vkb708b0q59b6imypyjlgbiigyb74rcfq"))

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

(define rust-encoding-rs-0.8.35
  (crate-source "encoding_rs" "0.8.35"
                "1wv64xdrr9v37rqqdjsyb8l8wzlcbab80ryxhrszvnj59wy0y0vm"))

(define rust-endi-1.1.0
  (crate-source "endi" "1.1.0"
                "1gxp388g2zzbncp3rdn60wxkr49xbhhx94nl9p4a6c41w4ma7n53"))

(define rust-endi-1.1.1
  (crate-source "endi" "1.1.1"
                "16a0076dx41vgrzzimm9clcym77h732czqjiajanmzvd1i1y5dv6"))

(define rust-enumflags2-0.7.10
  (crate-source "enumflags2" "0.7.10"
                "0g8kmhaqxq44v76wh971biljrgaqbjc8fbyw2d1z3wsnb5zxncnj"))

(define rust-enumflags2-0.7.12
  (crate-source "enumflags2" "0.7.12"
                "1vzcskg4dca2jiflsfx1p9yw1fvgzcakcs7cpip0agl51ilgf9qh"))

(define rust-enumflags2-derive-0.7.10
  (crate-source "enumflags2_derive" "0.7.10"
                "1s29iqx3gj5l5s19a22lpn0nljhp5l9smqac99126n2qhfhlh3fy"))

(define rust-enumflags2-derive-0.7.12
  (crate-source "enumflags2_derive" "0.7.12"
                "09rqffacafl1b83ir55hrah9gza0x7pzjn6lr6jm76fzix6qmiv7"))

(define rust-env-filter-0.1.2
  (crate-source "env_filter" "0.1.2"
                "1avnnd60ig6q5ixxxbwicwkxpqjg8bl9x7qn8c7wbvkcvb794b2g"))

(define rust-env-filter-0.1.4
  (crate-source "env_filter" "0.1.4"
                "1qk8yn4lsqzxsz025kf4kaabika6aidykqih3c2p1jjms9cw5wqv"))

(define rust-env-logger-0.10.1
  (crate-source "env_logger" "0.10.1"
                "1kmy9xmfjaqfvd4wkxr1f7d16ld3h9b487vqs2q9r0s8f3kg7cwm"))

(define rust-env-logger-0.11.5
  (crate-source "env_logger" "0.11.5"
                "13812wq68kybv2vj6rpnhbl7ammlhggcb7vq68bkichzp4cscgz1"))

(define rust-env-logger-0.11.8
  (crate-source "env_logger" "0.11.8"
                "17q6zbjam4wq75fa3m4gvvmv3rj3ch25abwbm84b28a0j3q67j0k"))

(define rust-equivalent-1.0.1
  (crate-source "equivalent" "1.0.1"
                "1malmx5f4lkfvqasz319lq6gb3ddg19yzf9s8cykfsgzdmyq0hsl"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-errno-0.3.14
  (crate-source "errno" "0.3.14"
                "1szgccmh8vgryqyadg8xd58mnwwicf39zmin3bsn63df2wbbgjir"))

(define rust-errno-0.3.8
  (crate-source "errno" "0.3.8"
                "0ia28ylfsp36i27g1qih875cyyy4by2grf80ki8vhgh6vinf8n52"))

(define rust-errno-0.3.9
  (crate-source "errno" "0.3.9"
                "1fi0m0493maq1jygcf1bya9cymz2pc1mqxj26bdv7yjd37v5qk2k"))

(define rust-error-code-3.3.2
  (crate-source "error-code" "3.3.2"
                "0nacxm9xr3s1rwd6fabk3qm89fyglahmbi4m512y0hr8ym6dz8ny"))

(define rust-etagere-0.2.15
  (crate-source "etagere" "0.2.15"
                "0hjkik00nl8bm9fafpzqkb6yah3mkphc21zpc159a5fwwncvz2gw"))

(define rust-etcetera-0.10.0
  (crate-source "etcetera" "0.10.0"
                "1rka6bskn93pdhx32xaagr147q95z5bnz7ym5xr85jw00wyv3ir6"))

(define rust-euclid-0.22.13
  (crate-source "euclid" "0.22.13"
                "0qzdj4xicyrsvd4cq6m5ndvfdrvvqrawy799qbaqhzw37r4byqfz"))

(define rust-evdev-0.13.1
  (crate-source "evdev" "0.13.1"
                "1y5q8xdf8ppgcgnkbwjnnqkm4zycimp09mn2nfcp66mbmrjhihd3"))

(define rust-event-listener-5.3.1
  (crate-source "event-listener" "5.3.1"
                "1fkm6q4hjn61wl52xyqyyxai0x9w0ngrzi0wf1qsf8vhsadvwck0"))

(define rust-event-listener-5.4.1
  (crate-source "event-listener" "5.4.1"
                "1asnp3agbr8shcl001yd935m167ammyi8hnvl0q1ycajryn6cfz1"))

(define rust-event-listener-strategy-0.5.2
  (crate-source "event-listener-strategy" "0.5.2"
                "18f5ri227khkayhv3ndv7yl4rnasgwksl2jhwgafcxzr7324s88g"))

(define rust-event-listener-strategy-0.5.4
  (crate-source "event-listener-strategy" "0.5.4"
                "14rv18av8s7n8yixg38bxp5vg2qs394rl1w052by5npzmbgz7scb"))

(define rust-fastrand-2.0.1
  (crate-source "fastrand" "2.0.1"
                "19flpv5zbzpf0rk4x77z4zf25in0brg8l7m304d3yrf47qvwxjr5"))

(define rust-fastrand-2.2.0
  (crate-source "fastrand" "2.2.0"
                "1i0sp22gv8n4h4w5cf10l2b3rfdi0da2kp0d4hl7jw65fdp80vs8"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-fdeflate-0.3.7
  (crate-source "fdeflate" "0.3.7"
                "130ga18vyxbb5idbgi07njymdaavvk6j08yh1dfarm294ssm6s0y"))

(define rust-find-msvc-tools-0.1.0
  (crate-source "find-msvc-tools" "0.1.0"
                "0l4nnivhdigxd87drmahqq99qvz7479ad65syq1njwm2m3xy8y71"))

(define rust-find-msvc-tools-0.1.5
  (crate-source "find-msvc-tools" "0.1.5"
                "0i1ql02y37bc7xywkqz10kx002vpz864vc4qq88h1jam190pcc1s"))

(define rust-find-msvc-tools-0.1.9
  (crate-source "find-msvc-tools" "0.1.9"
                "10nmi0qdskq6l7zwxw5g56xny7hb624iki1c39d907qmfh3vrbjv"))

(define rust-flate2-1.1.5
  (crate-source "flate2" "1.1.5"
                "1yrvxgxyg7mzksmmcd9i7vc3023kbv3zhdsf8mkjm8c5ivfkxqxz"))

(define rust-flate2-1.1.9
  (crate-source "flate2" "1.1.9"
                "0g2pb7cxnzcbzrj8bw4v6gpqqp21aycmf6d84rzb6j748qkvlgw4"))

(define rust-float-cmp-0.9.0
  (crate-source "float-cmp" "0.9.0"
                "1i799ksbq7fj9rm9m82g1yqgm6xi3jnrmylddmqknmksajylpplq"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-foldhash-0.1.5
  (crate-source "foldhash" "0.1.5"
                "1wisr1xlc2bj7hk4rgkcjkz3j2x4dhd1h9lwk7mj8p71qpdgbi6r"))

(define rust-foldhash-0.2.0
  (crate-source "foldhash" "0.2.0"
                "1nvgylb099s11xpfm1kn2wcsql080nqmnhj1l25bp3r2b35j9kkp"))

(define rust-font-types-0.10.1
  (crate-source "font-types" "0.10.1"
                "1idqs7pfv9134fzkfykjcg5x34gfjclpqqccln7d9jxv0ks599ir"))

(define rust-fontconfig-parser-0.5.8
  (crate-source "fontconfig-parser" "0.5.8"
                "0ijnbzg31sl6v49g7q2l7sl76hjj8z0hvlsz77cdvm029vi77ixv"))

(define rust-fontdb-0.23.0
  (crate-source "fontdb" "0.23.0"
                "0199vry9x8zn9ix4x4rqvv53dy2ryhy68l53jwr580hj7ndphzj5"))

(define rust-foreign-types-0.3.2
  (crate-source "foreign-types" "0.3.2"
                "1cgk0vyd7r45cj769jym4a6s7vwshvd0z4bqrb92q1fwibmkkwzn"))

(define rust-foreign-types-0.5.0
  (crate-source "foreign-types" "0.5.0"
                "0rfr2zfxnx9rz3292z5nyk8qs2iirznn5ff3rd4vgdwza6mdjdyp"))

(define rust-foreign-types-macros-0.2.3
  (crate-source "foreign-types-macros" "0.2.3"
                "0hjpii8ny6l7h7jpns2cp9589016l8mlrpaigcnayjn9bdc6qp0s"))

(define rust-foreign-types-shared-0.1.1
  (crate-source "foreign-types-shared" "0.1.1"
                "0jxgzd04ra4imjv8jgkmdq59kj8fsz6w4zxsbmlai34h26225c00"))

(define rust-foreign-types-shared-0.3.1
  (crate-source "foreign-types-shared" "0.3.1"
                "0nykdvv41a3d4py61bylmlwjhhvdm0b3bcj9vxhqgxaxnp5ik6ma"))

(define rust-form-urlencoded-1.2.2
  (crate-source "form_urlencoded" "1.2.2"
                "1kqzb2qn608rxl3dws04zahcklpplkd5r1vpabwga5l50d2v4k6b"))

(define rust-funty-2.0.0
  (crate-source "funty" "2.0.0"
                "177w048bm0046qlzvp33ag3ghqkqw4ncpzcm5lq36gxf2lla7mg6"))

(define rust-futures-0.3.31
  (crate-source "futures" "0.3.31"
                "0xh8ddbkm9jy8kc5gbvjp9a4b6rqqxvc8471yb2qaz5wm2qhgg35"))

(define rust-futures-channel-0.3.31
  (crate-source "futures-channel" "0.3.31"
                "040vpqpqlbk099razq8lyn74m0f161zd0rp36hciqrwcg2zibzrd"))

(define rust-futures-core-0.3.31
  (crate-source "futures-core" "0.3.31"
                "0gk6yrxgi5ihfanm2y431jadrll00n5ifhnpx090c2f2q1cr1wh5"))

(define rust-futures-executor-0.3.31
  (crate-source "futures-executor" "0.3.31"
                "17vcci6mdfzx4gbk0wx64chr2f13wwwpvyf3xd5fb1gmjzcx2a0y"))

(define rust-futures-io-0.3.31
  (crate-source "futures-io" "0.3.31"
                "1ikmw1yfbgvsychmsihdkwa8a1knank2d9a8dk01mbjar9w1np4y"))

(define rust-futures-lite-2.6.0
  (crate-source "futures-lite" "2.6.0"
                "0cmmgszlmkwsac9pyw5rfjakmshgx4wmzmlyn6mmjs0jav4axvgm"))

(define rust-futures-lite-2.6.1
  (crate-source "futures-lite" "2.6.1"
                "1ba4dg26sc168vf60b1a23dv1d8rcf3v3ykz2psb7q70kxh113pp"))

(define rust-futures-macro-0.3.31
  (crate-source "futures-macro" "0.3.31"
                "0l1n7kqzwwmgiznn0ywdc5i24z72zvh9q1dwps54mimppi7f6bhn"))

(define rust-futures-sink-0.3.31
  (crate-source "futures-sink" "0.3.31"
                "1xyly6naq6aqm52d5rh236snm08kw8zadydwqz8bip70s6vzlxg5"))

(define rust-futures-task-0.3.31
  (crate-source "futures-task" "0.3.31"
                "124rv4n90f5xwfsm9qw6y99755y021cmi5dhzh253s920z77s3zr"))

(define rust-futures-util-0.3.31
  (crate-source "futures-util" "0.3.31"
                "10aa1ar8bgkgbr4wzxlidkqkcxf77gffyj8j7768h831pcaq784z"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-gethostname-1.1.0
  (crate-source "gethostname" "1.1.0"
                "1n6bj9gh503ggjblfjcai96gmxynxsrykaynljlrfdra34q95m0v"))

(define rust-getrandom-0.2.16
  (crate-source "getrandom" "0.2.16"
                "14l5aaia20cc6cc08xdlhrzmfcylmrnprwnna20lqf746pqzjprk"))

(define rust-getrandom-0.3.4
  (crate-source "getrandom" "0.3.4"
                "1zbpvpicry9lrbjmkd4msgj3ihff1q92i334chk7pzf46xffz7c9"))

(define rust-getrandom-0.4.1
  (crate-source "getrandom" "0.4.1"
                "1v7fm84f2jh6x7w3bd2ncl3sw29wnb0rhg7xya1pd30i02cg77hk"))

(define rust-gif-0.13.3
  (crate-source "gif" "0.13.3"
                "06z6gll24q7psbz9fb86jbcbmgwnxkym8jsp0fbq5qikbqilgq2a"))

(define rust-gimli-0.31.1
  (crate-source "gimli" "0.31.1"
                "0gvqc0ramx8szv76jhfd4dms0zyamvlg4whhiz11j34hh3dqxqh7"))

(define rust-gl-generator-0.14.0
  (crate-source "gl_generator" "0.14.0"
                "0k8j1hmfnff312gy7x1aqjzcm8zxid7ij7dlb8prljib7b1dz58s"))

(define rust-glam-0.25.0
  (crate-source "glam" "0.25.0"
                "1cr80mz1qyc73fvp2d93d4g2svv675fmcrkrzi0bpyajpvcna5hm"))

(define rust-glow-0.16.0
  (crate-source "glow" "0.16.0"
                "022z12nlyfpy36fvp2szq792xix1xbgkznpmicf1c404sxhfmrf5"))

(define rust-glutin-wgl-sys-0.6.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "glutin_wgl_sys" "0.6.1"
                "0gng2810jb5x133lmy17qifjx6s90lnprm86afg7mfls505y0kic"))

(define rust-gpu-alloc-0.6.0
  (crate-source "gpu-alloc" "0.6.0"
                "0wd1wq7qs8ja0cp37ajm9p1r526sp6w0kvjp3xx24jsrjfx2vkgv"))

(define rust-gpu-alloc-types-0.3.0
  (crate-source "gpu-alloc-types" "0.3.0"
                "190wxsp9q8c59xybkfrlzqqyrxj6z39zamadk1q7v0xad2s07zwq"))

(define rust-gpu-allocator-0.27.0
  (crate-source "gpu-allocator" "0.27.0"
                "1kfa6kvvslmrr63p4n5fsm8myp03xi5lzylywys9f0l0xyjs4lf1"))

(define rust-gpu-descriptor-0.3.2
  (crate-source "gpu-descriptor" "0.3.2"
                "1jm0acxkw9lrzzcbvjqynwdr53qsqz7vx5d8c8h77qq5j4s8775q"))

(define rust-gpu-descriptor-types-0.2.0
  (crate-source "gpu-descriptor-types" "0.2.0"
                "14ab90klss7w0ybj95fcnqxjsjya17xjhf576dpvi4zq5ml45wpx"))

(define rust-guillotiere-0.6.2
  (crate-source "guillotiere" "0.6.2"
                "10m7fhp5kzf09kz08k6apkbzblriyqynjl1wwa9i7jrnq1jmhbdn"))

(define rust-gumdrop-0.8.1
  (crate-source "gumdrop" "0.8.1"
                "1qr94qa0h758hn11yhqs2wmb1xaq8adjs8j6hljg1xnji7wh1isv"))

(define rust-gumdrop-derive-0.8.1
  (crate-source "gumdrop_derive" "0.8.1"
                "17d91ai4p9f9cwhqqnyivw9yi7prl9xzpaqq3a1yfxwx8k9rp7vj"))

(define rust-h2-0.4.12
  (crate-source "h2" "0.4.12"
                "11hk5mpid8757z6n3v18jwb62ikffrgzjlrgpzqvkqdlzjfbdh7k"))

(define rust-half-2.7.1
  (crate-source "half" "2.7.1"
                "0jyq42xfa6sghc397mx84av7fayd4xfxr4jahsqv90lmjr5xi8kf"))

(define rust-harfrust-0.3.2
  (crate-source "harfrust" "0.3.2"
                "1n5r45i2f8w71ia9vql7c8da1qxd7j3hfxpylc98l7f72bdj1h4j"))

(define rust-hashbrown-0.15.2
  (crate-source "hashbrown" "0.15.2"
                "12dj0yfn59p3kh3679ac0w1fagvzf4z2zp87a13gbbqbzw0185dz"))

(define rust-hashbrown-0.15.5
  (crate-source "hashbrown" "0.15.5"
                "189qaczmjxnikm9db748xyhiw04kpmhm9xj9k9hg0sgx7pjwyacj"))

(define rust-hashbrown-0.16.1
  (crate-source "hashbrown" "0.16.1"
                "004i3njw38ji3bzdp9z178ba9x3k0c1pgy8x69pj7yfppv4iq7c4"))

(define rust-heck-0.4.1
  (crate-source "heck" "0.4.1"
                "1a7mqsnycv5z4z5vnv1k34548jzmc0ajic7c1j8jsaspnhw5ql4m"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hermit-abi-0.3.3
  (crate-source "hermit-abi" "0.3.3"
                "1dyc8qsjh876n74a3rcz8h43s27nj1sypdhsn2ms61bd3b47wzyp"))

(define rust-hermit-abi-0.3.9
  (crate-source "hermit-abi" "0.3.9"
                "092hxjbjnq5fmz66grd9plxd0sh6ssg5fhgwwwqbrzgzkjwdycfj"))

(define rust-hermit-abi-0.4.0
  (crate-source "hermit-abi" "0.4.0"
                "1k1zwllx6nfq417hy38x4akw1ivlv68ymvnzyxs76ffgsqcskxpv"))

(define rust-hermit-abi-0.5.2
  (crate-source "hermit-abi" "0.5.2"
                "1744vaqkczpwncfy960j2hxrbjl1q01csm84jpd9dajbdr2yy3zw"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

(define rust-hexf-parse-0.2.1
  (crate-source "hexf-parse" "0.2.1"
                "1pr3a3sk66ddxdyxdxac7q6qaqjcn28v0njy22ghdpfn78l8d9nz"))

(define rust-hmac-0.12.1
  (crate-source "hmac" "0.12.1"
                "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"))

(define rust-home-0.5.12
  (crate-source "home" "0.5.12"
                "13bjyzgx6q9srnfvl43dvmhn93qc8mh5w7cylk2g13sj3i3pyqnc"))

(define rust-home-0.5.9
  (crate-source "home" "0.5.9"
                "19grxyg35rqfd802pcc9ys1q3lafzlcjcv2pl2s5q8xpyr5kblg3"))

(define rust-http-1.4.0
  (crate-source "http" "1.4.0"
                "06iind4cwsj1d6q8c2xgq8i2wka4ps74kmws24gsi1bzdlw2mfp3"))

(define rust-http-body-1.0.1
  (crate-source "http-body" "1.0.1"
                "111ir5k2b9ihz5nr9cz7cwm7fnydca7dx4hc7vr16scfzghxrzhy"))

(define rust-http-body-util-0.1.3
  (crate-source "http-body-util" "0.1.3"
                "0jm6jv4gxsnlsi1kzdyffjrj8cfr3zninnxpw73mvkxy4qzdj8dh"))

(define rust-httparse-1.10.1
  (crate-source "httparse" "1.10.1"
                "11ycd554bw2dkgw0q61xsa7a4jn1wb1xbfacmf3dbwsikvkkvgvd"))

(define rust-humantime-2.1.0
  (crate-source "humantime" "2.1.0"
                "1r55pfkkf5v0ji1x6izrjwdq9v6sc7bv99xj6srywcar37xmnfls"))

(define rust-hyper-1.8.1
  (crate-source "hyper" "1.8.1"
                "04cxr8j5y86bhxxlyqb8xkxjskpajk7cxwfzzk4v3my3a3rd9cia"))

(define rust-hyper-rustls-0.27.7
  (crate-source "hyper-rustls" "0.27.7"
                "0n6g8998szbzhnvcs1b7ibn745grxiqmlpg53xz206v826v3xjg3"))

(define rust-hyper-tls-0.6.0
  (crate-source "hyper-tls" "0.6.0"
                "1q36x2yps6hhvxq5r7mc8ph9zz6xlb573gx0x3yskb0fi736y83h"))

(define rust-hyper-util-0.1.19
  (crate-source "hyper-util" "0.1.19"
                "0pyzc8378baf996l5ycl4y0s3skhxc4z4vkah9mvff3r1vb0ay3j"))

(define rust-iced-0.14.0
  (crate-source "iced" "0.14.0"
                "1cqzf3fzcwn4j696yzrpb8k5ar8fv8xdp8sphczn9flkdh1023h0"))

(define rust-iced-core-0.14.0
  (crate-source "iced_core" "0.14.0"
                "11cxf8scyvx4r722q1dj9agyxg02m51yfai5d5z3wh4rsqvikawi"))

(define rust-iced-debug-0.14.0
  (crate-source "iced_debug" "0.14.0"
                "1gm05qhb89xd625c7gz42wlgp8ar9vy3cghhyi9hwqjs46q5l0r5"))

(define rust-iced-futures-0.14.0
  (crate-source "iced_futures" "0.14.0"
                "02nz7wm85xmhl2ls8dyiab6dp80fmwcc0diw573vxps2mp68a34c"))

(define rust-iced-graphics-0.14.0
  (crate-source "iced_graphics" "0.14.0"
                "00m18kxq58xwb6i81393r1m4jb14np8zm9cgyram05f4rv1a2k13"))

(define rust-iced-program-0.14.0
  (crate-source "iced_program" "0.14.0"
                "0wx6whk9s61y6knmlcznkq7sc6m1gcrsq3dhis6ninkwjk1gxykd"))

(define rust-iced-renderer-0.14.0
  (crate-source "iced_renderer" "0.14.0"
                "01dcbnp2l7ssg7yvk92qwqz44py60xynribfk1vw1s084j0c0315"))

(define rust-iced-runtime-0.14.0
  (crate-source "iced_runtime" "0.14.0"
                "1m589zs8dk07vha6jfa1aik6b56lr0vf6hij31s6dh74kj0rp26i"))

(define rust-iced-tiny-skia-0.14.0
  (crate-source "iced_tiny_skia" "0.14.0"
                "0gfcx5i1qbr3ppja1p99xbm6xcy1zzyjj8szzx593g53fn5wy2py"))

(define rust-iced-wgpu-0.14.0
  (crate-source "iced_wgpu" "0.14.0"
                "04qwaqqaxj79pq7fql7989f84h020q04b4sp0ahzi80ckfcll57z"))

(define rust-iced-widget-0.14.2
  (crate-source "iced_widget" "0.14.2"
                "1aspx7drf0sprvgl4p7rz16hdlqidjp2phcbirhw429i1px6lndi"))

(define rust-iced-winit-0.14.0
  (crate-source "iced_winit" "0.14.0"
                "0x9d2qixfzbsiyhvayjb021c724bcygr6zbhp7iisban8zfbwzcb"))

(define rust-icu-collections-2.1.1
  (crate-source "icu_collections" "2.1.1"
                "0hsblchsdl64q21qwrs4hvc2672jrf466zivbj1bwyv606bn8ssc"))

(define rust-icu-locale-core-2.1.1
  (crate-source "icu_locale_core" "2.1.1"
                "1djvdc2f5ylmp1ymzv4gcnmq1s4hqfim9nxlcm173lsd01hpifpd"))

(define rust-icu-normalizer-2.1.1
  (crate-source "icu_normalizer" "2.1.1"
                "16dmn5596la2qm0r3vih0bzjfi0vx9a20yqjha6r1y3vnql8hv2z"))

(define rust-icu-normalizer-data-2.1.1
  (crate-source "icu_normalizer_data" "2.1.1"
                "02jnzizg6q75m41l6c13xc7nkc5q8yr1b728dcgfhpzw076wrvbs"))

(define rust-icu-properties-2.1.2
  (crate-source "icu_properties" "2.1.2"
                "1v3lbmhhi7i6jgw51ikjb1p50qh5rb67grlkdnkc63l7zq1gq2q2"))

(define rust-icu-properties-data-2.1.2
  (crate-source "icu_properties_data" "2.1.2"
                "1bvpkh939rgzrjfdb7hz47v4wijngk0snmcgrnpwc9fpz162jv31"))

(define rust-icu-provider-2.1.1
  (crate-source "icu_provider" "2.1.1"
                "0576b7dizgyhpfa74kacv86y4g1p7v5ffd6c56kf1q82rvq2r5l5"))

(define rust-id-arena-2.3.0
  (crate-source "id-arena" "2.3.0"
                "0m6rs0jcaj4mg33gkv98d71w3hridghp5c4yr928hplpkgbnfc1x"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-idna-1.1.0
  (crate-source "idna" "1.1.0"
                "1pp4n7hppm480zcx411dsv9wfibai00wbpgnjj4qj0xa7kr7a21v"))

(define rust-idna-adapter-1.2.1
  (crate-source "idna_adapter" "1.2.1"
                "0i0339pxig6mv786nkqcxnwqa87v4m94b2653f6k3aj0jmhfkjis"))

(define rust-image-webp-0.2.4
  (crate-source "image-webp" "0.2.4"
                "1hz814csyi9283vinzlkix6qpnd6hs3fkw7xl6z2zgm4w7rrypjj"))

(define rust-imagesize-0.13.0
  (crate-source "imagesize" "0.13.0"
                "11f26ac9zvbr7sjnsv2z9jd3ryaz40pg8xch4ij1q1rg5zbjgkgd"))

(define rust-indexmap-2.12.1
  (crate-source "indexmap" "2.12.1"
                "1wmcfk7g7d9wz1dninlijx70kfkzz6d5r36nyi2hdjjvaqmvpm0a"))

(define rust-indexmap-2.13.0
  (crate-source "indexmap" "2.13.0"
                "05qh5c4h2hrnyypphxpwflk45syqbzvqsvvyxg43mp576w2ff53p"))

(define rust-indexmap-2.6.0
  (crate-source "indexmap" "2.6.0"
                "1nmrwn8lbs19gkvhxaawffzbvrpyrb5y3drcrr645x957kz0fybh"))

(define rust-inout-0.1.4
  (crate-source "inout" "0.1.4"
                "008xfl1jn9rxsq19phnhbimccf4p64880jmnpg59wqi07kk117w7"))

(define rust-io-kit-sys-0.4.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "io-kit-sys" "0.4.1"
                "0ysy5k3wf54yangy25hkj10xx332cj2hb937xasg6riziv7yczk1"))

(define rust-io-lifetimes-1.0.11
  (crate-source "io-lifetimes" "1.0.11"
                "1hph5lz4wd3drnn6saakwxr497liznpfnv70via6s0v8x6pbkrza"))

(define rust-ipnet-2.11.0
  (crate-source "ipnet" "2.11.0"
                "0c5i9sfi2asai28m8xp48k5gvwkqrg5ffpi767py6mzsrswv17s6"))

(define rust-iri-string-0.7.9
  (crate-source "iri-string" "0.7.9"
                "15s3s6k99ci52d7qdplhllpa6xyvdyiys645n6z6fsw93nfpp1jg"))

(define rust-is-terminal-0.4.10
  (crate-source "is-terminal" "0.4.10"
                "0m9la3f7cs77y85nkbcjsxkb7k861fc6bdhahyfidgh7gljh1b8b"))

(define rust-is-terminal-polyfill-1.70.1
  (crate-source "is_terminal_polyfill" "1.70.1"
                "1kwfgglh91z33kl0w5i338mfpa3zs0hidq5j4ny4rmjwrikchhvr"))

(define rust-is-terminal-polyfill-1.70.2
  (crate-source "is_terminal_polyfill" "1.70.2"
                "15anlc47sbz0jfs9q8fhwf0h3vs2w4imc030shdnq54sny5i7jx6"))

(define rust-itoa-1.0.10
  (crate-source "itoa" "1.0.10"
                "0k7xjfki7mnv6yzjrbnbnjllg86acmbnk4izz2jmm1hx2wd6v95i"))

(define rust-itoa-1.0.14
  (crate-source "itoa" "1.0.14"
                "0x26kr9m062mafaxgcf2p6h2x7cmixm0zw95aipzn2hr3d5jlnnp"))

(define rust-itoa-1.0.15
  (crate-source "itoa" "1.0.15"
                "0b4fj9kz54dr3wam0vprjwgygvycyw8r0qwg7vp19ly8b2w16psa"))

(define rust-itoa-1.0.17
  (crate-source "itoa" "1.0.17"
                "1lh93xydrdn1g9x547bd05g0d3hra7pd1k4jfd2z1pl1h5hwdv4j"))

(define rust-jiff-0.2.16
  (crate-source "jiff" "0.2.16"
                "0ddvdlmg7a3glbbq70hj6jfyraxplvhc4ny3xziyg6103ywf5k29"))

(define rust-jiff-0.2.19
  (crate-source "jiff" "0.2.19"
                "15a4w1jgx9n5i9g5lmv7jzzdcmgn4n1dajqzbmpavafm21g5p6nq"))

(define rust-jiff-static-0.2.16
  (crate-source "jiff-static" "0.2.16"
                "0sgsa0cgx2p036x37lj279srz0vhh7n6gqdc979xim9s7jsgh2lq"))

(define rust-jiff-static-0.2.19
  (crate-source "jiff-static" "0.2.19"
                "1gsd0cbxc6rmy1cl6w44cizqqnfl5pqsiw6c2n93dh9ghv43jypz"))

(define rust-jni-0.21.1
  (crate-source "jni" "0.21.1"
                "15wczfkr2r45slsljby12ymf2hij8wi5b104ghck9byjnwmsm1qs"))

(define rust-jni-sys-0.3.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "jni-sys" "0.3.0"
                "0c01zb9ygvwg9wdx2fii2d39myzprnpqqhy7yizxvjqp5p04pbwf"))

(define rust-jobserver-0.1.34
  (crate-source "jobserver" "0.1.34"
                "0cwx0fllqzdycqn4d6nb277qx5qwnmjdxdl0lxkkwssx77j3vyws"))

(define rust-js-sys-0.3.83
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.83"
                "1n71vpxrzclly0530lwkcsx6mg73lipam2ak3rr1ypzmqw4kfjj6"))

(define rust-js-sys-0.3.85
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "js-sys" "0.3.85"
                "1csmb42fxjmzjdgc790bgw77sf1cb9ydm5rdsnh5qj4miszjx54c"))

(define rust-khronos-api-3.1.0
  (crate-source "khronos_api" "3.1.0"
                "1p0xj5mlbagqyvvnv8wmv3cr7l9y1m153888pxqwg3vk3mg5inz2"))

(define rust-khronos-egl-6.0.0
  (crate-source "khronos-egl" "6.0.0"
                "0xnzdx0n1bil06xmh8i1x6dbxvk7kd2m70bbm6nw1qzc43r1vbka"))

(define rust-kurbo-0.10.4
  (crate-source "kurbo" "0.10.4"
                "0h1lqqdxv2xd5lyvpfsw01hyjpzkms03sdndwxkpvs93v7mx860n"))

(define rust-kurbo-0.11.3
  (crate-source "kurbo" "0.11.3"
                "0qiwq4l83fy9v5d1piywvh44yg9ha3rl04d2kdcqlvvm8jp2c866"))

(define rust-lazy-static-1.4.0
  (crate-source "lazy_static" "1.4.0"
                "0in6ikhw8mgl33wjv6q6xfrb5b9jr16q8ygjy803fay4zcisvaz2"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-leb128fmt-0.1.0
  (crate-source "leb128fmt" "0.1.0"
                "1chxm1484a0bly6anh6bd7a99sn355ymlagnwj3yajafnpldkv89"))

(define rust-libbz2-rs-sys-0.2.2
  ;; TODO: Check bundled sources.
  (crate-source "libbz2-rs-sys" "0.2.2"
                "1xz88pa6xc372kjnr9gv4qaz5myjna9d7db5a2a7sk142md58jic"))

(define rust-libc-0.2.153
  (crate-source "libc" "0.2.153"
                "1gg7m1ils5dms5miq9fyllrcp0jxnbpgkx71chd2i0lafa8qy6cw"))

(define rust-libc-0.2.166
  (crate-source "libc" "0.2.166"
                "0dnyxi6n3h72cq79grmvc2vcmh13v1y4x1k1s0dk7cf0pc4c3k62"))

(define rust-libc-0.2.175
  (crate-source "libc" "0.2.175"
                "0hw5sb3gjr0ivah7s3fmavlpvspjpd4mr009abmam2sr7r4sx0ka"))

(define rust-libc-0.2.178
  (crate-source "libc" "0.2.178"
                "1490yks6mria93i3xdva1gm05cjz824g14mbv0ph32lxma6kvj9p"))

(define rust-libc-0.2.181
  (crate-source "libc" "0.2.181"
                "1idib3j104yi5by6nclc1zhrakb53hm71jra2fdq771bmzi2g525"))

(define rust-libloading-0.8.8
  (crate-source "libloading" "0.8.8"
                "0rw6q94psj3d6k0bi9nymqhyrz78lbdblryphhaszsw9p9ikj0q7"))

(define rust-libloading-0.8.9
  (crate-source "libloading" "0.8.9"
                "0mfwxwjwi2cf0plxcd685yxzavlslz7xirss3b9cbrzyk4hv1i6p"))

(define rust-libm-0.2.16
  (crate-source "libm" "0.2.16"
                "10brh0a3qjmbzkr5mf5xqi887nhs5y9layvnki89ykz9xb1wxlmn"))

(define rust-libredox-0.1.12
  (crate-source "libredox" "0.1.12"
                "05h6fb2y05h74zwaafmnf7gv3bxilzp7syqlfzw524w55kh9a2rx"))

(define rust-libudev-sys-0.1.4
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "libudev-sys" "0.1.4"
                "09236fdzlx9l0dlrsc6xx21v5x8flpfm3d5rjq9jr5ivlas6k11w"))

(define rust-libz-rs-sys-0.5.4
  ;; TODO: Check bundled sources.
  (crate-source "libz-rs-sys" "0.5.4"
                "177kkz2022bcfjmbri60qwcr88iv4g5r3q6wcm6qv1md2pv3wh8m"))

(define rust-lilt-0.8.1
  (crate-source "lilt" "0.8.1"
                "1bap1s8l2vzyxbing3z2jh4l4s376l1wbqcvz99hbcpnxzjn4xgn"))

(define rust-linebender-resource-handle-0.1.1
  (crate-source "linebender_resource_handle" "0.1.1"
                "1x34mrmqan0m3m9xf2iy6vpkcx6vwiiyzm2g3ixqdi56rimzz9fl"))

(define rust-linux-raw-sys-0.11.0
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.11.0"
                "0fghx0nn8nvbz5yzgizfcwd6ap2pislp68j8c1bwyr6sacxkq7fz"))

(define rust-linux-raw-sys-0.4.12
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.12"
                "0mhlla3gk1jgn6mrq9s255rvvq8a1w3yk2vpjiwsd6hmmy1imkf4"))

(define rust-linux-raw-sys-0.4.14
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.14"
                "12gsjgbhhjwywpqcrizv80vrp7p7grsz5laqq773i33wphjsxcvq"))

(define rust-linux-raw-sys-0.4.15
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.15"
                "1aq7r2g7786hyxhv40spzf2nhag5xbw2axxc1k8z5k1dsgdm4v6j"))

(define rust-linux-raw-sys-0.9.4
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.9.4"
                "04kyjdrq79lz9ibrf7czk6cv9d3jl597pb9738vzbsbzy1j5i56d"))

(define rust-litemap-0.8.1
  (crate-source "litemap" "0.8.1"
                "0xsy8pfp9s802rsj1bq2ys2kbk1g36w5dr3gkfip7gphb5x60wv3"))

(define rust-litrs-1.0.0
  (crate-source "litrs" "1.0.0"
                "14p0kzzkavnngvybl88nvfwv031cc2qx4vaxpfwsiifm8grdglqi"))

(define rust-lock-api-0.4.14
  (crate-source "lock_api" "0.4.14"
                "0rg9mhx7vdpajfxvdjmgmlyrn20ligzqvn8ifmaz7dc79gkrjhr2"))

(define rust-log-0.4.20
  (crate-source "log" "0.4.20"
                "13rf7wphnwd61vazpxr7fiycin6cb1g8fmvgqg18i464p0y1drmm"))

(define rust-log-0.4.22
  (crate-source "log" "0.4.22"
                "093vs0wkm1rgyykk7fjbqp2lwizbixac1w52gv109p5r4jh0p9x7"))

(define rust-log-0.4.27
  (crate-source "log" "0.4.27"
                "150x589dqil307rv0rwj0jsgz5bjbwvl83gyl61jf873a7rjvp0k"))

(define rust-log-0.4.29
  (crate-source "log" "0.4.29"
                "15q8j9c8g5zpkcw0hnd6cf2z7fxqnvsjh3rw5mv5q10r83i34l2y"))

(define rust-logind-zbus-5.2.0
  (crate-source "logind-zbus" "5.2.0"
                "1rsxjsz8jdipw5jkrsw29xq5v37a01k478rwqb3ncz99fc651nwa"))

(define rust-lru-0.16.3
  (crate-source "lru" "0.16.3"
                "14z5yxcp3f63lgw8yxr486g9yz7cfqbmkadfwgw36vy0jbslgp51"))

(define rust-lzma-rust2-0.13.0
  (crate-source "lzma-rust2" "0.13.0"
                "0jlaq6b1c1vrv1nlg16bf7qggqj68yqlc4ig34ipwlhdp7zj62n6"))

(define rust-mach2-0.4.3
  (crate-source "mach2" "0.4.3"
                "0i6vcnbq5v51whgyidzhf7cbxqrmj2nkw8z0m2ib02rc60mjhh6n"))

(define rust-malloc-buf-0.0.6
  (crate-source "malloc_buf" "0.0.6"
                "1jqr77j89pwszv51fmnknzvd53i1nkmcr8rjrvcxhm4dx1zr1fv2"))

(define rust-memchr-2.7.1
  (crate-source "memchr" "2.7.1"
                "0jf1kicqa4vs9lyzj4v4y1p90q0dh87hvhsdd5xvhnp527sw8gaj"))

(define rust-memchr-2.7.4
  (crate-source "memchr" "2.7.4"
                "18z32bhxrax0fnjikv475z7ii718hq457qwmaryixfxsl2qrmjkq"))

(define rust-memchr-2.7.5
  (crate-source "memchr" "2.7.5"
                "1h2bh2jajkizz04fh047lpid5wgw2cr9igpkdhl3ibzscpd858ij"))

(define rust-memchr-2.7.6
  (crate-source "memchr" "2.7.6"
                "0wy29kf6pb4fbhfksjbs05jy2f32r2f3r1ga6qkmpz31k79h0azm"))

(define rust-memchr-2.8.0
  (crate-source "memchr" "2.8.0"
                "0y9zzxcqxvdqg6wyag7vc3h0blhdn7hkq164bxyx2vph8zs5ijpq"))

(define rust-memmap2-0.9.9
  (crate-source "memmap2" "0.9.9"
                "146lfx0mpib44wvws6hibahm4h2w867bzwsc6zhmi9p0l3j36hbl"))

(define rust-memoffset-0.7.1
  (crate-source "memoffset" "0.7.1"
                "1x2zv8hv9c9bvgmhsjvr9bymqwyxvgbca12cm8xkhpyy5k1r7s2x"))

(define rust-memoffset-0.9.1
  (crate-source "memoffset" "0.9.1"
                "12i17wh9a9plx869g7j4whf62xw68k5zd4k0k5nh6ys5mszid028"))

(define rust-metal-0.32.0
  (crate-source "metal" "0.32.0"
                "01g6a10l2p576ibybz0v9hignw7qj0r6d513qbf2vxrzcxpmmh80"))

(define rust-mime-0.3.17
  (crate-source "mime" "0.3.17"
                "16hkibgvb9klh0w0jk5crr5xv90l3wlf77ggymzjmvl1818vnxv8"))

(define rust-miniz-oxide-0.8.0
  (crate-source "miniz_oxide" "0.8.0"
                "1wadxkg6a6z4lr7kskapj5d8pxlx7cp1ifw4daqnkzqjxych5n72"))

(define rust-miniz-oxide-0.8.9
  (crate-source "miniz_oxide" "0.8.9"
                "05k3pdg8bjjzayq3rf0qhpirq9k37pxnasfn4arbs17phqn6m9qz"))

(define rust-mio-1.0.2
  (crate-source "mio" "1.0.2"
                "1v1cnnn44awxbcfm4zlavwgkvbyg7gp5zzjm8mqf1apkrwflvq40"))

(define rust-mio-1.1.1
  (crate-source "mio" "1.1.1"
                "1z2phpalqbdgihrcjp8y09l3kgq6309jnhnr6h11l9s7mnqcm6x6"))

(define rust-mundy-0.2.2
  (crate-source "mundy" "0.2.2"
                "12vy5rlllwhdc0w7zdhm44w9j7sm2aqlw88571ll7v4lw74i6f2j"))

(define rust-naga-27.0.3
  (crate-source "naga" "27.0.3"
                "1n57m5hwi42mk57ljz7manc45b8ky8890891y86yw4cb1rgz4v06"))

(define rust-native-tls-0.2.14
  (crate-source "native-tls" "0.2.14"
                "03hga800x8bzkp8h7frnm7yp545dwwawgmaq673vx7byk1139pl7"))

(define rust-ndk-0.9.0
  (crate-source "ndk" "0.9.0"
                "1m32zpmi5w1pf3j47k6k5fw395dc7aj8d0mdpsv53lqkprxjxx63"))

(define rust-ndk-context-0.1.1
  (crate-source "ndk-context" "0.1.1"
                "12sai3dqsblsvfd1l1zab0z6xsnlha3xsfl7kagdnmj3an3jvc17"))

(define rust-ndk-sys-0.6.0+11769913
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "ndk-sys" "0.6.0+11769913"
                "0wx8r6pji20if4xs04g73gxl98nmjrfc73z0v6w1ypv6a4qdlv7f"))

(define rust-nix-0.26.4
  (crate-source "nix" "0.26.4"
                "06xgl4ybb8pvjrbmc3xggbgk3kbs1j0c4c0nzdfrmpbgrkrym2sr"))

(define rust-nix-0.29.0
  (crate-source "nix" "0.29.0"
                "0ikvn7s9r2lrfdm3mx1h7nbfjvcc6s9vxdzw7j5xfkd2qdnp9qki"))

(define rust-num-conv-0.1.0
  (crate-source "num-conv" "0.1.0"
                "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))

(define rust-num-cpus-1.17.0
  (crate-source "num_cpus" "1.17.0"
                "0fxjazlng4z8cgbmsvbzv411wrg7x3hyxdq8nxixgzjswyylppwi"))

(define rust-num-enum-0.7.5
  (crate-source "num_enum" "0.7.5"
                "0k25hagf3xfgmj4j1zmvja1d6844jrmpginxpd3vhmxd41z7l85i"))

(define rust-num-enum-derive-0.7.5
  (crate-source "num_enum_derive" "0.7.5"
                "1mx4dgza8b9g16baybc00gg06jn4cf17h45p0fr3qx5nw5fkccpz"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-nusb-0.2.1
  (crate-source "nusb" "0.2.1"
                "0gvnd44q8fzcybxzr86p95j7z1r2fxv17xvwfhhghy7fnd6ny8nh"))

(define rust-objc-0.2.7
  (crate-source "objc" "0.2.7"
                "1cbpf6kz8a244nn1qzl3xyhmp05gsg4n313c9m3567625d3innwi"))

(define rust-objc-sys-0.3.5
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "objc-sys" "0.3.5"
                "0423gry7s3rmz8s3pzzm1zy5mdjif75g6dbzc2lf2z0c77fipffd"))

(define rust-objc2-0.5.2
  (crate-source "objc2" "0.5.2"
                "015qa2d3vh7c1j2736h5wjrznri7x5ic35vl916c22gzxva8b9s6"))

(define rust-objc2-0.6.3
  (crate-source "objc2" "0.6.3"
                "01ccrb558qav2rqrmk0clzqzdd6r1rmicqnf55xqam7cw2f5khmp"))

(define rust-objc2-app-kit-0.2.2
  (crate-source "objc2-app-kit" "0.2.2"
                "1zqyi5l1bm26j1bgmac9783ah36m5kcrxlqp5carglnpwgcrms74"))

(define rust-objc2-app-kit-0.3.2
  (crate-source "objc2-app-kit" "0.3.2"
                "132ijwni8lsi8phq7wnmialkxp46zx998fns3zq5np0ya1mr77nl"))

(define rust-objc2-cloud-kit-0.2.2
  (crate-source "objc2-cloud-kit" "0.2.2"
                "02dhjvmcq8c2bwj31jx423jygif1scs9f0lmlab0ayhw75b3ppbl"))

(define rust-objc2-cloud-kit-0.3.2
  (crate-source "objc2-cloud-kit" "0.3.2"
                "0714xrydi9wvh25s2110sjfpx9mv4xs9p4ys71q8fhxvh3c79bbk"))

(define rust-objc2-contacts-0.2.2
  (crate-source "objc2-contacts" "0.2.2"
                "12a8m927xrrxa54xhqhqnkkl1a6l07pyrpnqfk9jz09kkh755zx5"))

(define rust-objc2-core-data-0.2.2
  (crate-source "objc2-core-data" "0.2.2"
                "1vvk8zjylfjjj04dzawydmqqz5ajvdkhf22cnb07ihbiw14vyzv1"))

(define rust-objc2-core-data-0.3.2
  (crate-source "objc2-core-data" "0.3.2"
                "1ylqsa6hpma7k4090pkil8b7c0i8dcxnh46zwhnfidgv7rjjlh0b"))

(define rust-objc2-core-foundation-0.3.2
  (crate-source "objc2-core-foundation" "0.3.2"
                "0dnmg7606n4zifyjw4ff554xvjmi256cs8fpgpdmr91gckc0s61a"))

(define rust-objc2-core-graphics-0.3.2
  (crate-source "objc2-core-graphics" "0.3.2"
                "01x8413pxq0m5rwidlaczni8v5cz9dc3xqzq8l9zlpl9cv8cj8p0"))

(define rust-objc2-core-image-0.2.2
  (crate-source "objc2-core-image" "0.2.2"
                "102csfb82zi2sbzliwsfd589ckz0gysf7y6434c9zj97lmihj9jm"))

(define rust-objc2-core-image-0.3.2
  (crate-source "objc2-core-image" "0.3.2"
                "01phi7cx2k32a8x45qr0y1623l2b8gg764c6isgj15rbinrn7mg5"))

(define rust-objc2-core-location-0.2.2
  (crate-source "objc2-core-location" "0.2.2"
                "10apgsrigqryvi4rcc0f6yfjflvrl83f4bi5hkr48ck89vizw300"))

(define rust-objc2-core-text-0.3.2
  (crate-source "objc2-core-text" "0.3.2"
                "0bfrzqxhgh4y1imk1bb9g0v28g0frigls6hnc942npfj93xhvphc"))

(define rust-objc2-core-video-0.3.2
  (crate-source "objc2-core-video" "0.3.2"
                "19j1a7f863gh30nq03w70x1js3f3vdg3wp4azllky8vkvzqwl9fl"))

(define rust-objc2-encode-4.1.0
  (crate-source "objc2-encode" "4.1.0"
                "0cqckp4cpf68mxyc2zgnazj8klv0z395nsgbafa61cjgsyyan9gg"))

(define rust-objc2-foundation-0.2.2
  (crate-source "objc2-foundation" "0.2.2"
                "1a6mi77jsig7950vmx9ydvsxaighzdiglk5d229k569pvajkirhf"))

(define rust-objc2-foundation-0.3.2
  (crate-source "objc2-foundation" "0.3.2"
                "0wijkxzzvw2xkzssds3fj8279cbykz2rz9agxf6qh7y2agpsvq73"))

(define rust-objc2-io-surface-0.3.2
  (crate-source "objc2-io-surface" "0.3.2"
                "07fqx4fmwydf2arrc4xs4awv7zyzzxh60fyqdfmrpm9n148qh1qq"))

(define rust-objc2-link-presentation-0.2.2
  (crate-source "objc2-link-presentation" "0.2.2"
                "160k4qh00yrx57dabn3hzas4r98kmk9bc0qsy1jvwday3irax8d1"))

(define rust-objc2-metal-0.2.2
  (crate-source "objc2-metal" "0.2.2"
                "1mmdga66qpxrcfq3gxxhysfx3zg1hpx4z886liv3j0pnfq9bl36x"))

(define rust-objc2-quartz-core-0.2.2
  (crate-source "objc2-quartz-core" "0.2.2"
                "0ynw8819c36l11rim8n0yzk0fskbzrgaqayscyqi8swhzxxywaz4"))

(define rust-objc2-quartz-core-0.3.2
  (crate-source "objc2-quartz-core" "0.3.2"
                "07vzaf6y1lk7zygkgvpp23mm19ipdm9yq8af22gvywdkaa23bhcn"))

(define rust-objc2-symbols-0.2.2
  (crate-source "objc2-symbols" "0.2.2"
                "1p04hjkxan18g2b7h9n2n8xxsvazapv2h6mfmmdk06zc7pz4ws0a"))

(define rust-objc2-ui-kit-0.2.2
  (crate-source "objc2-ui-kit" "0.2.2"
                "0vrb5r8z658l8c19bx78qks8c5hg956544yirf8npk90idwldfxq"))

(define rust-objc2-uniform-type-identifiers-0.2.2
  (crate-source "objc2-uniform-type-identifiers" "0.2.2"
                "1ziv4wkbxcaw015ypg0q49ycl7m14l3x56mpq2k1rznv92bmzyj4"))

(define rust-objc2-user-notifications-0.2.2
  (crate-source "objc2-user-notifications" "0.2.2"
                "1cscv2w3vxzaslz101ddv0z9ycrrs4ayikk4my4qd3im8bvcpkvn"))

(define rust-object-0.36.5
  (crate-source "object" "0.36.5"
                "0gk8lhbs229c68lapq6w6qmnm4jkj48hrcw5ilfyswy514nhmpxf"))

(define rust-once-cell-1.19.0
  (crate-source "once_cell" "1.19.0"
                "14kvw7px5z96dk4dwdm1r9cqhhy2cyj1l5n5b29mynbb8yr15nrz"))

(define rust-once-cell-1.20.2
  (crate-source "once_cell" "1.20.2"
                "0xb7rw1aqr7pa4z3b00y7786gyf8awx2gca3md73afy76dzgwq8j"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-once-cell-polyfill-1.70.2
  (crate-source "once_cell_polyfill" "1.70.2"
                "1zmla628f0sk3fhjdjqzgxhalr2xrfna958s632z65bjsfv8ljrq"))

(define rust-openssl-0.10.75
  (crate-source "openssl" "0.10.75"
                "0a238gvrzjq0r62a7472i685hi5jjzgfj72kp1xd32ir46qqv0q8"))

(define rust-openssl-macros-0.1.1
  (crate-source "openssl-macros" "0.1.1"
                "173xxvfc63rr5ybwqwylsir0vq6xsj4kxiv4hmg4c3vscdmncj59"))

(define rust-openssl-probe-0.1.6
  (crate-source "openssl-probe" "0.1.6"
                "0bl52x55laalqb707k009h8kfawliwp992rlsvkzy49n47p2fpnh"))

(define rust-openssl-sys-0.9.111
  ;; TODO: Check bundled sources.
  (crate-source "openssl-sys" "0.9.111"
                "08f3mpsabivfi3fd0qv9231qidqy68lr8a4qi32y6xda43av5jl2"))

(define rust-orbclient-0.3.50
  (crate-source "orbclient" "0.3.50"
                "1p8z477y3f7258gabbnv145ih71svndwac6cs6jpl2vhmrmjrbaj"))

(define rust-ordered-float-4.6.0
  (crate-source "ordered-float" "4.6.0"
                "0ldrcgilsiijd141vw51fbkziqmh5fpllil3ydhirjm67wdixdvv"))

(define rust-ordered-stream-0.2.0
  (crate-source "ordered-stream" "0.2.0"
                "0l0xxp697q7wiix1gnfn66xsss7fdhfivl2k7bvpjs4i3lgb18ls"))

(define rust-owned-ttf-parser-0.25.1
  (crate-source "owned_ttf_parser" "0.25.1"
                "0fsqzcbc4sq8qhkmc3rgcfg1xg389nmhlxvmvi6h38dca680x0in"))

(define rust-papergrid-0.17.0
  (crate-source "papergrid" "0.17.0"
                "1wg3k8kgv0rgxqszf5c6dv6347mm58qb5kii0q4g9n2iif614y39"))

(define rust-parking-2.2.1
  (crate-source "parking" "2.2.1"
                "1fnfgmzkfpjd69v4j9x737b1k8pnn054bvzcn5dm3pkgq595d3gk"))

(define rust-parking-lot-0.12.5
  (crate-source "parking_lot" "0.12.5"
                "06jsqh9aqmc94j2rlm8gpccilqm6bskbd67zf6ypfc0f4m9p91ck"))

(define rust-parking-lot-core-0.9.12
  (crate-source "parking_lot_core" "0.9.12"
                "1hb4rggy70fwa1w9nb0svbyflzdc69h047482v2z3sx2hmcnh896"))

(define rust-paste-1.0.15
  (crate-source "paste" "1.0.15"
                "02pxffpdqkapy292harq6asfjvadgp1s005fip9ljfsn9fvxgh2p"))

(define rust-pbkdf2-0.12.2
  (crate-source "pbkdf2" "0.12.2"
                "1wms79jh4flpy1zi8xdp4h8ccxv4d85adc6zjagknvppc5vnmvgq"))

(define rust-percent-encoding-2.3.2
  (crate-source "percent-encoding" "2.3.2"
                "083jv1ai930azvawz2khv7w73xh8mnylk7i578cifndjn5y64kwv"))

(define rust-pico-args-0.5.0
  (crate-source "pico-args" "0.5.0"
                "05d30pvxd6zlnkg2i3ilr5a70v3f3z2in18m67z25vinmykngqav"))

(define rust-pin-project-1.1.10
  (crate-source "pin-project" "1.1.10"
                "12kadbnfm1f43cyadw9gsbyln1cy7vj764wz5c8wxaiza3filzv7"))

(define rust-pin-project-internal-1.1.10
  (crate-source "pin-project-internal" "1.1.10"
                "0qgqzfl0f4lzaz7yl5llhbg97g68r15kljzihaw9wm64z17qx4bf"))

(define rust-pin-project-lite-0.2.15
  (crate-source "pin-project-lite" "0.2.15"
                "1zz4xif3iknfrpmvqmh0pcc9mx4cxm28jywqydir3pimcla1wnli"))

(define rust-pin-project-lite-0.2.16
  (crate-source "pin-project-lite" "0.2.16"
                "16wzc7z7dfkf9bmjin22f5282783f6mdksnr0nv0j5ym5f9gyg1v"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

(define rust-piper-0.2.4
  (crate-source "piper" "0.2.4"
                "0rn0mjjm0cwagdkay77wgmz3sqf8fqmv9d9czm79mvr2yj8c9j4n"))

(define rust-pkg-config-0.3.31
  (crate-source "pkg-config" "0.3.31"
                "1wk6yp2phl91795ia0lwkr3wl4a9xkrympvhqq8cxk4d75hwhglm"))

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

(define rust-png-0.17.16
  (crate-source "png" "0.17.16"
                "09kmkms9fmkbkarw0lnf0scqvjwwg3r7riddag0i3q39r0pil5c2"))

(define rust-polling-3.11.0
  (crate-source "polling" "3.11.0"
                "0622qfbxi3gb0ly2c99n3xawp878fkrd1sl83hjdhisx11cly3jx"))

(define rust-polling-3.7.4
  (crate-source "polling" "3.7.4"
                "0bs4nhwfwsvlzlhah2gbhj3aa9ynvchv2g350wapswh26a65c156"))

(define rust-portable-atomic-1.11.1
  (crate-source "portable-atomic" "1.11.1"
                "10s4cx9y3jvw0idip09ar52s2kymq8rq9a668f793shn1ar6fhpq"))

(define rust-portable-atomic-1.13.1
  (crate-source "portable-atomic" "1.13.1"
                "0j8vlar3n5acyigq8q6f4wjx3k3s5yz0rlpqrv76j73gi5qr8fn3"))

(define rust-portable-atomic-util-0.2.4
  (crate-source "portable-atomic-util" "0.2.4"
                "01rmx1li07ixsx3sqg2bxqrkzk7b5n8pibwwf2589ms0s3cg18nq"))

(define rust-portable-atomic-util-0.2.5
  (crate-source "portable-atomic-util" "0.2.5"
                "1xcm0ia8756k6hdgafx4g3lx3fw0hvz2zqswq7c2sy58gxnvk7bs"))

(define rust-potential-utf-0.1.4
  (crate-source "potential_utf" "0.1.4"
                "0xxg0pkfpq299wvwln409z4fk80rbv55phh3f1jhjajy5x1ljfdp"))

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

(define rust-ppmd-rust-1.3.0
  (crate-source "ppmd-rust" "1.3.0"
                "1d0p6zak4qina673rrlw1h4w2s44ydz027vslbr1c3s5y1cwan6m"))

(define rust-presser-0.3.1
  (crate-source "presser" "0.3.1"
                "1ykvqx861sjmhkdh540aafqba7i7li7gqgwrcczy6v56i9m8xkz8"))

(define rust-pretty-assertions-1.4.1
  (crate-source "pretty_assertions" "1.4.1"
                "0v8iq35ca4rw3rza5is3wjxwsf88303ivys07anc5yviybi31q9s"))

(define rust-prettyplease-0.2.37
  (crate-source "prettyplease" "0.2.37"
                "0azn11i1kh0byabhsgab6kqs74zyrg69xkirzgqyhz6xmjnsi727"))

(define rust-proc-macro-crate-3.2.0
  (crate-source "proc-macro-crate" "3.2.0"
                "0yzsqnavb3lmrcsmbrdjfrky9vcbl46v59xi9avn0796rb3likwf"))

(define rust-proc-macro-crate-3.4.0
  (crate-source "proc-macro-crate" "3.4.0"
                "10v9qi51n4phn1lrj5r94kjq7yhci9jrkqnn6wpan05yjsgb3711"))

(define rust-proc-macro-error-attr2-2.0.0
  (crate-source "proc-macro-error-attr2" "2.0.0"
                "1ifzi763l7swl258d8ar4wbpxj4c9c2im7zy89avm6xv6vgl5pln"))

(define rust-proc-macro-error2-2.0.1
  (crate-source "proc-macro-error2" "2.0.1"
                "00lq21vgh7mvyx51nwxwf822w2fpww1x0z8z0q47p8705g2hbv0i"))

(define rust-proc-macro2-1.0.101
  (crate-source "proc-macro2" "1.0.101"
                "1pijhychkpl7rcyf1h7mfk6gjfii1ywf5n0snmnqs5g4hvyl7bl9"))

(define rust-proc-macro2-1.0.103
  (crate-source "proc-macro2" "1.0.103"
                "1s29bz20xl2qk5ffs2mbdqknaj43ri673dz86axdbf47xz25psay"))

(define rust-proc-macro2-1.0.106
  (crate-source "proc-macro2" "1.0.106"
                "0d09nczyaj67x4ihqr5p7gxbkz38gxhk4asc0k8q23g9n85hzl4g"))

(define rust-proc-macro2-1.0.72
  (crate-source "proc-macro2" "1.0.72"
                "1i99vh7bqgyrcxj77awzmrg5p38a754ir8nj3bn7hr6g2s1k34x2"))

(define rust-proc-macro2-1.0.92
  (crate-source "proc-macro2" "1.0.92"
                "1c1vjy5wg8iy7kxsxda564qf4ljp0asysmbn2i7caj177x5m9lrp"))

(define rust-profiling-1.0.17
  (crate-source "profiling" "1.0.17"
                "0wqp6i1bl7azy9270dp92srbbr55mgdh9qnk5b1y44lyarmlif1y"))

(define rust-quick-error-2.0.1
  (crate-source "quick-error" "2.0.1"
                "18z6r2rcjvvf8cn92xjhm2qc3jpd1ljvcbf12zv0k9p565gmb4x9"))

(define rust-quick-xml-0.28.2
  (crate-source "quick-xml" "0.28.2"
                "1lfr3512x0s0i9kbyglyzn0rq0i1bvd2mqqfi8gs685808rfgr8c"))

(define rust-quick-xml-0.38.4
  (crate-source "quick-xml" "0.38.4"
                "0772siy4d9vlq77842012c8cycs3y0szxkv62rh9sh2sqmc20v5n"))

(define rust-quote-1.0.33
  (crate-source "quote" "1.0.33"
                "1biw54hbbr12wdwjac55z1m2x2rylciw83qnjn564a3096jgqrsj"))

(define rust-quote-1.0.37
  (crate-source "quote" "1.0.37"
                "1brklraw2g34bxy9y4q1nbrccn7bv36ylihv12c9vlcii55x7fdm"))

(define rust-quote-1.0.40
  (crate-source "quote" "1.0.40"
                "1394cxjg6nwld82pzp2d4fp6pmaz32gai1zh9z5hvh0dawww118q"))

(define rust-quote-1.0.42
  (crate-source "quote" "1.0.42"
                "0zq6yc7dhpap669m27rb4qfbiywxfah17z6fwvfccv3ys90wqf53"))

(define rust-quote-1.0.44
  (crate-source "quote" "1.0.44"
                "1r7c7hxl66vz3q9qizgjhy77pdrrypqgk4ghc7260xvvfb7ypci1"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-radium-0.7.0
  (crate-source "radium" "0.7.0"
                "02cxfi3ky3c4yhyqx9axqwhyaca804ws46nn4gc1imbk94nzycyw"))

(define rust-range-alloc-0.1.4
  (crate-source "range-alloc" "0.1.4"
                "1plvrb6gaaa5in2fjv67wgs9aki8qrczz77qcjhqw2d5ccb87mn3"))

(define rust-rangemap-1.7.1
  (crate-source "rangemap" "1.7.1"
                "0s7am2w72siggn668h03gn3g06gsinv6m1jaaxmnbj59177l6d4p"))

(define rust-raw-window-handle-0.6.2
  (crate-source "raw-window-handle" "0.6.2"
                "0ff5c648hncwx7hm2a8fqgqlbvbl4xawb6v3xxv9wkpjyrr5arr0"))

(define rust-read-fonts-0.35.0
  (crate-source "read-fonts" "0.35.0"
                "0n0kr71apps034i708r6vx3y2kd35da2j4vikm5zdbc8nhiwy5v7"))

(define rust-redox-syscall-0.4.1
  (crate-source "redox_syscall" "0.4.1"
                "1aiifyz5dnybfvkk4cdab9p2kmphag1yad6iknc7aszlxxldf8j7"))

(define rust-redox-syscall-0.5.18
  (crate-source "redox_syscall" "0.5.18"
                "0b9n38zsxylql36vybw18if68yc9jczxmbyzdwyhb9sifmag4azd"))

(define rust-redox-syscall-0.7.0
  (crate-source "redox_syscall" "0.7.0"
                "09zfw2jp6hgpn5pkayv9wh01sw410566qk8zwkljm7p6i44gxws9"))

(define rust-regex-1.10.2
  (crate-source "regex" "1.10.2"
                "0hxkd814n4irind8im5c9am221ri6bprx49nc7yxv02ykhd9a2rq"))

(define rust-regex-1.11.1
  (crate-source "regex" "1.11.1"
                "148i41mzbx8bmq32hsj1q4karkzzx5m60qza6gdw4pdc9qdyyi5m"))

(define rust-regex-1.12.2
  (crate-source "regex" "1.12.2"
                "1m14zkg6xmkb0q5ah3y39cmggclsjdr1wpxfa4kf5wvm3wcw0fw4"))

(define rust-regex-1.12.3
  (crate-source "regex" "1.12.3"
                "0xp2q0x7ybmpa5zlgaz00p8zswcirj9h8nry3rxxsdwi9fhm81z1"))

(define rust-regex-automata-0.4.13
  (crate-source "regex-automata" "0.4.13"
                "070z0j23pjfidqz0z89id1fca4p572wxpcr20a0qsv68bbrclxjj"))

(define rust-regex-automata-0.4.14
  (crate-source "regex-automata" "0.4.14"
                "13xf7hhn4qmgfh784llcp2kzrvljd13lb2b1ca0mwnf15w9d87bf"))

(define rust-regex-automata-0.4.3
  (crate-source "regex-automata" "0.4.3"
                "0gs8q9yhd3kcg4pr00ag4viqxnh5l7jpyb9fsfr8hzh451w4r02z"))

(define rust-regex-automata-0.4.9
  (crate-source "regex-automata" "0.4.9"
                "02092l8zfh3vkmk47yjc8d631zhhcd49ck2zr133prvd3z38v7l0"))

(define rust-regex-syntax-0.8.2
  (crate-source "regex-syntax" "0.8.2"
                "17rd2s8xbiyf6lb4aj2nfi44zqlj98g2ays8zzj2vfs743k79360"))

(define rust-regex-syntax-0.8.5
  (crate-source "regex-syntax" "0.8.5"
                "0p41p3hj9ww7blnbwbj9h7rwxzxg0c1hvrdycgys8rxyhqqw859b"))

(define rust-regex-syntax-0.8.8
  (crate-source "regex-syntax" "0.8.8"
                "0n7ggnpk0r32rzgnycy5xrc1yp2kq19m6pz98ch3c6dkaxw9hbbs"))

(define rust-regex-syntax-0.8.9
  (crate-source "regex-syntax" "0.8.9"
                "0k0a47r1rcl794wj8a948niakbg081s5pp5nlgcbmmr2iy3qfs59"))

(define rust-renderdoc-sys-1.1.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "renderdoc-sys" "1.1.0"
                "0cj8zjs7k0gvchcx3jhpg8r9bbqy8b1hsgbz0flcq2ydn12hmcqr"))

(define rust-reqwest-0.12.25
  (crate-source "reqwest" "0.12.25"
                "06khlxs7xw9pkvyawk9a0097795nkvbl47cipm1is4s0ilrgkvxn"))

(define rust-resvg-0.45.1
  (crate-source "resvg" "0.45.1"
                "0hyz89wyasn0wdnpw7qy5iyl4xv3yx36hk3crb4h6pm5q2c8g4m8"))

(define rust-rgb-0.8.52
  (crate-source "rgb" "0.8.52"
                "1km115a9lblf9pldvx51dmmg30y8ms4ka67hvas2ndcq556qhshc"))

(define rust-ring-0.17.14
  (crate-source "ring" "0.17.14"
                "1dw32gv19ccq4hsx3ribhpdzri1vnrlcfqb2vj41xn4l49n9ws54"))

(define rust-roxmltree-0.20.0
  (crate-source "roxmltree" "0.20.0"
                "15vw91ps91wkmmgy62khf9zb63bdinvm80957dascbsw7dwvc83c"))

(define rust-rustc-demangle-0.1.24
  (crate-source "rustc-demangle" "0.1.24"
                "07zysaafgrkzy2rjgwqdj2a8qdpsm6zv6f5pgpk9x0lm40z9b6vi"))

(define rust-rustc-hash-1.1.0
  (crate-source "rustc-hash" "1.1.0"
                "1qkc5khrmv5pqi5l5ca9p5nl5hs742cagrndhbrlk3dhlrx3zm08"))

(define rust-rustc-hash-2.1.1
  (crate-source "rustc-hash" "2.1.1"
                "03gz5lvd9ghcwsal022cgkq67dmimcgdjghfb5yb5d352ga06xrm"))

(define rust-rustc-version-0.2.3
  (crate-source "rustc_version" "0.2.3"
                "02h3x57lcr8l2pm0a645s9whdh33pn5cnrwvn5cb57vcrc53x3hk"))

(define rust-rustix-0.38.32
  (crate-source "rustix" "0.38.32"
                "12fvzwnsb13svnqzsf01maz44dib8kmgp2w8cxp7f8azwrhliq35"))

(define rust-rustix-0.38.41
  (crate-source "rustix" "0.38.41"
                "1xi4caglazqny7qv933l96ninjy8a68ygpkfg8b5wjf15f8lkxnp"))

(define rust-rustix-0.38.44
  (crate-source "rustix" "0.38.44"
                "0m61v0h15lf5rrnbjhcb9306bgqrhskrqv7i1n0939dsw8dbrdgx"))

(define rust-rustix-1.1.2
  (crate-source "rustix" "1.1.2"
                "0gpz343xfzx16x82s1x336n0kr49j02cvhgxdvaq86jmqnigh5fd"))

(define rust-rustix-1.1.3
  (crate-source "rustix" "1.1.3"
                "0d0z2zcw4rwzni1hm8snw8xdxwwrij336m31c4ghq66cghj9wv0l"))

(define rust-rustls-0.23.35
  (crate-source "rustls" "0.23.35"
                "13xxk2qqchibd7pr0laqq6pzayx9xm4rb45d8rz68kvxday58gsk"))

(define rust-rustls-pki-types-1.13.1
  (crate-source "rustls-pki-types" "1.13.1"
                "134hjrwxzrkiag11psaml4gv75f4a9m307cc8rr05fjlbyfhz33h"))

(define rust-rustls-webpki-0.103.8
  (crate-source "rustls-webpki" "0.103.8"
                "0lpymb84bi5d2pm017n39nbiaa5cd046hgz06ir29ql6a8pzmz9g"))

(define rust-rustversion-1.0.22
  (crate-source "rustversion" "1.0.22"
                "0vfl70jhv72scd9rfqgr2n11m5i9l1acnk684m2w83w0zbqdx75k"))

(define rust-rustybuzz-0.20.1
  (crate-source "rustybuzz" "0.20.1"
                "00hp1gwykjfli258zs7lqg8p2zdh94dv2mw8zx7f73m0z2b7qg7x"))

(define rust-ryu-1.0.16
  (crate-source "ryu" "1.0.16"
                "0k7b90xr48ag5bzmfjp82rljasw2fx28xr3bg1lrpx7b5sljm3gr"))

(define rust-ryu-1.0.18
  (crate-source "ryu" "1.0.18"
                "17xx2s8j1lln7iackzd9p0sv546vjq71i779gphjq923vjh5pjzk"))

(define rust-ryu-1.0.20
  (crate-source "ryu" "1.0.20"
                "07s855l8sb333h6bpn24pka5sp7hjk2w667xy6a0khkf6sqv5lr8"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-schannel-0.1.28
  (crate-source "schannel" "0.1.28"
                "1qb6s5gyxfz2inz753a4z3mc1d266mwvz0c5w7ppd3h44swq27c9"))

(define rust-scoped-tls-1.0.1
  (crate-source "scoped-tls" "1.0.1"
                "15524h04mafihcvfpgxd8f4bgc3k95aclz8grjkg9a0rxcvn9kz1"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-sctk-adwaita-0.10.1
  (crate-source "sctk-adwaita" "0.10.1"
                "1v14vqp7k39jk7pgaibwc06qq9vcmi82k7zlv3qpfvq52w17y9xn"))

(define rust-security-framework-2.11.1
  (crate-source "security-framework" "2.11.1"
                "00ldclwx78dm61v7wkach9lcx76awlrv0fdgjdwch4dmy12j4yw9"))

(define rust-security-framework-sys-2.15.0
  ;; TODO: Check bundled sources.
  (crate-source "security-framework-sys" "2.15.0"
                "1h6mijxnfrwvl1y4dzwn3m877j6dqp9qn3g37i954j5czazhq7yc"))

(define rust-self-cell-1.2.2
  (crate-source "self_cell" "1.2.2"
                "12cdmh9p2h72rmw923kj841jji4k0vrykihvx19fn059az8pcbmi"))

(define rust-semver-0.9.0
  (crate-source "semver" "0.9.0"
                "00q4lkcj0rrgbhviv9sd4p6qmdsipkwkbra7rh11jrhq5kpvjzhx"))

(define rust-semver-1.0.27
  (crate-source "semver" "1.0.27"
                "1qmi3akfrnqc2hfkdgcxhld5bv961wbk8my3ascv5068mc5fnryp"))

(define rust-semver-parser-0.7.0
  (crate-source "semver-parser" "0.7.0"
                "18vhypw6zgccnrlm5ps1pwa0khz7ry927iznpr88b87cagr1v2iq"))

(define rust-serde-1.0.193
  (crate-source "serde" "1.0.193"
                "129b0j67594f8qg5cbyi3nyk31y97wrqihi026mba34dwrsrkp95"))

(define rust-serde-1.0.215
  (crate-source "serde" "1.0.215"
                "13xqkw93cw9rnbkm0zy1apnilzq7l2xf1qw8m1nkga8i1fnw24v5"))

(define rust-serde-1.0.228
  (crate-source "serde" "1.0.228"
                "17mf4hhjxv5m90g42wmlbc61hdhlm6j9hwfkpcnd72rpgzm993ls"))

(define rust-serde-core-1.0.228
  (crate-source "serde_core" "1.0.228"
                "1bb7id2xwx8izq50098s5j2sqrrvk31jbbrjqygyan6ask3qbls1"))

(define rust-serde-derive-1.0.193
  (crate-source "serde_derive" "1.0.193"
                "1lwlx2k7wxr1v160kpyqjfabs37gm1yxqg65383rnyrm06jnqms3"))

(define rust-serde-derive-1.0.215
  (crate-source "serde_derive" "1.0.215"
                "1h2nswy0rmzblil38h12wxsgni1ik63rk22wy19g48v9hrpqc7md"))

(define rust-serde-derive-1.0.228
  (crate-source "serde_derive" "1.0.228"
                "0y8xm7fvmr2kjcd029g9fijpndh8csv5m20g4bd76w8qschg4h6m"))

(define rust-serde-json-1.0.108
  (crate-source "serde_json" "1.0.108"
                "0ssj59s7lpzqh1m50kfzlnrip0p0jg9lmhn4098i33a0mhz7w71x"))

(define rust-serde-json-1.0.133
  (crate-source "serde_json" "1.0.133"
                "0xz3bswa527wln3fy0qb7y081nx3cp5yy1ggjhi6n5mrfcjfpz67"))

(define rust-serde-json-1.0.145
  (crate-source "serde_json" "1.0.145"
                "1767y6kxjf7gwpbv8bkhgwc50nhg46mqwm9gy9n122f7v1k6yaj0"))

(define rust-serde-json-1.0.149
  (crate-source "serde_json" "1.0.149"
                "11jdx4vilzrjjd1dpgy67x5lgzr0laplz30dhv75lnf5ffa07z43"))

(define rust-serde-repr-0.1.19
  (crate-source "serde_repr" "0.1.19"
                "1sb4cplc33z86pzlx38234xr141wr3cmviqgssiadisgl8dlar3c"))

(define rust-serde-repr-0.1.20
  (crate-source "serde_repr" "0.1.20"
                "1755gss3f6lwvv23pk7fhnjdkjw7609rcgjlr8vjg6791blf6php"))

(define rust-serde-spanned-1.0.3
  (crate-source "serde_spanned" "1.0.3"
                "14j32cqcs6jjdl1c111lz6s0hr913dnmy2kpfd75k2761ym4ahz2"))

(define rust-serde-urlencoded-0.7.1
  (crate-source "serde_urlencoded" "0.7.1"
                "1zgklbdaysj3230xivihs30qi5vkhigg323a9m62k8jwf4a1qjfk"))

(define rust-sha1-0.10.6
  (crate-source "sha1" "0.10.6"
                "1fnnxlfg08xhkmwf2ahv634as30l1i3xhlhkvxflmasi5nd85gz3"))

(define rust-sha2-0.10.9
  (crate-source "sha2" "0.10.9"
                "10xjj843v31ghsksd9sl9y12qfc48157j1xpb8v1ml39jy0psl57"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-signal-hook-registry-1.4.2
  (crate-source "signal-hook-registry" "1.4.2"
                "1cb5akgq8ajnd5spyn587srvs4n26ryq0p78nswffwhv46sf1sd9"))

(define rust-signal-hook-registry-1.4.8
  (crate-source "signal-hook-registry" "1.4.8"
                "06vc7pmnki6lmxar3z31gkyg9cw7py5x9g7px70gy2hil75nkny4"))

(define rust-simd-adler32-0.3.8
  (crate-source "simd-adler32" "0.3.8"
                "18lx2gdgislabbvlgw5q3j5ssrr77v8kmkrxaanp3liimp2sc873"))

(define rust-simplecss-0.2.2
  (crate-source "simplecss" "0.2.2"
                "0v0kid7b2602kcka2x2xs9wwfjf8lnvpgpl8x287qg4wra1ni73s"))

(define rust-siphasher-1.0.2
  (crate-source "siphasher" "1.0.2"
                "13k7cfbpcm8qgj9p2n8dwg9skv9s0hxk5my30j5chy1p4l78bamj"))

(define rust-skrifa-0.37.0
  (crate-source "skrifa" "0.37.0"
                "0hcqlhgbiv5lhjv6l0hfj2gm1nabvdy9iv9zry77acpmxlfhfccc"))

(define rust-slab-0.4.11
  (crate-source "slab" "0.4.11"
                "12bm4s88rblq02jjbi1dw31984w61y2ldn13ifk5gsqgy97f8aks"))

(define rust-slab-0.4.12
  (crate-source "slab" "0.4.12"
                "1xcwik6s6zbd3lf51kkrcicdq2j4c1fw0yjdai2apy9467i0sy8c"))

(define rust-slab-0.4.9
  (crate-source "slab" "0.4.9"
                "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))

(define rust-slotmap-1.1.1
  (crate-source "slotmap" "1.1.1"
                "0f20xf53zaysx9ydzkwwqm6hsjyb8lj2j6amhg57iln3jcy8rmdx"))

(define rust-smallvec-1.11.2
  (crate-source "smallvec" "1.11.2"
                "0w79x38f7c0np7hqfmzrif9zmn0avjvvm31b166zdk9d1aad1k2d"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-smithay-client-toolkit-0.19.2
  (crate-source "smithay-client-toolkit" "0.19.2"
                "05h05hg4dn3v6br5jbdbs5nalk076a64s7fn6i01nqzby2hxwmrl"))

(define rust-smithay-client-toolkit-0.20.0
  (crate-source "smithay-client-toolkit" "0.20.0"
                "1h2cacmsh9zpw6sgmap49zx7cqhksfwas91mm40i5cz2ylwdl4h5"))

(define rust-smithay-clipboard-0.7.3
  (crate-source "smithay-clipboard" "0.7.3"
                "09hjm3dyjq4s3nxfzi65bg95hv540fi5zr5xad879xrryw1lqw3i"))

(define rust-smol-str-0.2.2
  (crate-source "smol_str" "0.2.2"
                "1bfylqf2vnqaglw58930vpxm2rfzji5gjp15a2c0kh8aj6v8ylyx"))

(define rust-socket2-0.5.7
  (crate-source "socket2" "0.5.7"
                "070r941wbq76xpy039an4pyiy3rfj7mp7pvibf1rcri9njq5wc6f"))

(define rust-socket2-0.6.1
  (crate-source "socket2" "0.6.1"
                "109qn0kjhqi5zds84qyqi5wn72g8azjhmf4b04fkgkrkd48rw4hp"))

(define rust-softbuffer-0.4.8
  (crate-source "softbuffer" "0.4.8"
                "1hznrcdzhhlr2x84926g28ybnlx649y1anr7mc4m3w5v3sl8vhda"))

(define rust-spirv-0.3.0+sdk-1.3.268.0
  (crate-source "spirv" "0.3.0+sdk-1.3.268.0"
                "0i3qj7yvvprai1s03dvll2gkfy8398nl64wvllkhaaa4vh1i197d"))

(define rust-stable-deref-trait-1.2.1
  (crate-source "stable_deref_trait" "1.2.1"
                "15h5h73ppqyhdhx6ywxfj88azmrpml9gl6zp3pwy2malqa6vxqkc"))

(define rust-static-assertions-1.1.0
  (crate-source "static_assertions" "1.1.0"
                "0gsl6xmw10gvn3zs1rv99laj5ig7ylffnh71f9l34js4nr4r7sx2"))

(define rust-strict-num-0.1.1
  (crate-source "strict-num" "0.1.1"
                "0cb7l1vhb8zj90mzm8avlk815k40sql9515s865rqdrdfavvldv6"))

(define rust-strsim-0.10.0
  (crate-source "strsim" "0.10.0"
                "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-strsim-0.9.3
  (crate-source "strsim" "0.9.3"
                "0k497pv882qn3q977ckznm13vxx927g8s1swvcv68j3c1pccwik4"))

(define rust-subtle-2.6.1
  (crate-source "subtle" "2.6.1"
                "14ijxaymghbl1p0wql9cib5zlwiina7kall6w7g89csprkgbvhhk"))

(define rust-svg-fmt-0.4.5
  (crate-source "svg_fmt" "0.4.5"
                "1yq2kv7klg8s5k19prn36cprlnb811cqgwhy079g7lng651wr4q1"))

(define rust-svgtypes-0.15.3
  (crate-source "svgtypes" "0.15.3"
                "1z4a0b76ww6rf2c8zdapqh2a7r7kmmy7m957q5h5ics4zwgm9iv8"))

(define rust-swash-0.2.6
  (crate-source "swash" "0.2.6"
                "02jmadp6nvmwrcn3028ks4h4gwnsnwjcryfh0zfnp5ry4n8n9127"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.111
  (crate-source "syn" "2.0.111"
                "11rf9l6435w525vhqmnngcnwsly7x4xx369fmaqvswdbjjicj31r"))

(define rust-syn-2.0.114
  (crate-source "syn" "2.0.114"
                "0akw62dizhyrkf3ym1jsys0gy1nphzgv0y8qkgpi6c1s4vghglfl"))

(define rust-syn-2.0.43
  (crate-source "syn" "2.0.43"
                "0lxdd5j4l904jlf9s7jahazyys10p07w2nry3x73cmfkyfsryrgf"))

(define rust-syn-2.0.89
  (crate-source "syn" "2.0.89"
                "0kicx48gndpxkkqnpv89f1slrzzbyldhr8nyhk6pmj61y6169m24"))

(define rust-sync-wrapper-1.0.2
  (crate-source "sync_wrapper" "1.0.2"
                "0qvjyasd6w18mjg5xlaq5jgy84jsjfsvmnn12c13gypxbv75dwhb"))

(define rust-synstructure-0.13.2
  (crate-source "synstructure" "0.13.2"
                "1lh9lx3r3jb18f8sbj29am5hm9jymvbwh6jb1izsnnxgvgrp12kj"))

(define rust-sys-locale-0.3.2
  (crate-source "sys-locale" "0.3.2"
                "1i16hq9mkwpzqvixjfy1ph4i2q5klgagjg4hibz6k894l2crmawf"))

(define rust-system-configuration-0.6.1
  (crate-source "system-configuration" "0.6.1"
                "0sxslml567zm0v8g732314vd2gk9sd3k4xj22xk6p64xir29v1rw"))

(define rust-system-configuration-sys-0.6.0
  ;; TODO: Check bundled sources.
  (crate-source "system-configuration-sys" "0.6.0"
                "1i5sqrmgy58l4704hibjbl36hclddglh73fb3wx95jnmrq81n7cf"))

(define rust-tabled-0.20.0
  (crate-source "tabled" "0.20.0"
                "0zawf8zg5frprmqjygldggn8zj4cyw0b7qbilw2hhdndzghjx6p3"))

(define rust-tabled-derive-0.11.0
  (crate-source "tabled_derive" "0.11.0"
                "0ii8l0l6fi8sbxrnir5mv91jbmzgbvqn5z8z4gwz3kx67jqx398f"))

(define rust-tap-1.0.1
  (crate-source "tap" "1.0.1"
                "0sc3gl4nldqpvyhqi3bbd0l9k7fngrcl4zs47n314nqqk4bpx4sm"))

(define rust-tempfile-3.14.0
  (crate-source "tempfile" "3.14.0"
                "037f9jm13bmfc6xq9w86dp0nylrddh6ynvl6db4gm1xwzi8y5k18"))

(define rust-tempfile-3.23.0
  (crate-source "tempfile" "3.23.0"
                "05igl2gml6z6i2va1bv49f9f1wb3f752c2i63lvlb9s2vxxwfc9d"))

(define rust-tempfile-3.25.0
  (crate-source "tempfile" "3.25.0"
                "1wg5jnzbgpb1wmw396v31f0c70dvj5mpik7rk7fzdccmghgpjdh1"))

(define rust-tempfile-3.9.0
  (crate-source "tempfile" "3.9.0"
                "1ypkl7rvv57n16q28psxpb61rnyhmfaif12ascdnsyljm90l3kh1"))

(define rust-termcolor-1.4.0
  (crate-source "termcolor" "1.4.0"
                "0jfllflbxxffghlq6gx4csv0bv0qv77943dcx01h9zssy39w66zz"))

(define rust-termcolor-1.4.1
  (crate-source "termcolor" "1.4.1"
                "0mappjh3fj3p2nmrg4y7qv94rchwi9mzmgmfflr8p2awdj7lyy86"))

(define rust-testing-table-0.3.0
  (crate-source "testing_table" "0.3.0"
                "1k0l036hgxmvjzr8ngc57ngkhnza3p9xh6cyc5jlz8lmk7iam38g"))

(define rust-thiserror-1.0.53
  (crate-source "thiserror" "1.0.53"
                "02fczinyqdp28gl6i7zsag577qiarw9bpp8kannhib9vfq25kkdj"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.17
  (crate-source "thiserror" "2.0.17"
                "1j2gixhm2c3s6g96vd0b01v0i0qz1101vfmw0032mdqj1z58fdgn"))

(define rust-thiserror-2.0.18
  (crate-source "thiserror" "2.0.18"
                "1i7vcmw9900bvsmay7mww04ahahab7wmr8s925xc083rpjybb222"))

(define rust-thiserror-impl-1.0.53
  (crate-source "thiserror-impl" "1.0.53"
                "06cywbwmsfs4l6x8wcrjhvb813jc4cj6zbiqdz6yl2nf9j14mkrx"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.17
  (crate-source "thiserror-impl" "2.0.17"
                "04y92yjwg1a4piwk9nayzjfs07sps8c4vq9jnsfq9qvxrn75rw9z"))

(define rust-thiserror-impl-2.0.18
  (crate-source "thiserror-impl" "2.0.18"
                "1mf1vrbbimj1g6dvhdgzjmn6q09yflz2b92zs1j9n3k7cxzyxi7b"))

(define rust-time-0.3.44
  (crate-source "time" "0.3.44"
                "179awlwb36zly3nmz5h9awai1h4pbf1d83g2pmvlw4v1pgixkrwi"))

(define rust-time-core-0.1.6
  (crate-source "time-core" "0.1.6"
                "0sqwhg7n47gbffyr0zhipqcnskxgcgzz1ix8wirqs2rg3my8x1j0"))

(define rust-tiny-skia-0.11.4
  (crate-source "tiny-skia" "0.11.4"
                "1aq9gd4qh4418g8v08qzakqqggx8hl66qcianl3k5bjdsja37lc3"))

(define rust-tiny-skia-path-0.11.4
  (crate-source "tiny-skia-path" "0.11.4"
                "14ywbdfakvacl6rxxmzbnycplaxpc6i2linh2yqk0sp8qb07z7lw"))

(define rust-tiny-xlib-0.2.4
  (crate-source "tiny-xlib" "0.2.4"
                "17nqhy48ab7vchxzplqrw2g88mx2zyr38kwr1ipan76hxx5m0903"))

(define rust-tinystr-0.8.2
  (crate-source "tinystr" "0.8.2"
                "0sa8z88axdsf088hgw5p4xcyi6g3w3sgbb6qdp81bph9bk2fkls2"))

(define rust-tinyvec-1.10.0
  (crate-source "tinyvec" "1.10.0"
                "1yhk0qdqyiaa4v2j9h8pzax5gxgwpz4da0lcphfil6g6pk1zv9dz"))

(define rust-tinyvec-macros-0.1.1
  (crate-source "tinyvec_macros" "0.1.1"
                "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z"))

(define rust-tokio-1.41.1
  (crate-source "tokio" "1.41.1"
                "0csdvrlpz2b0amrsinkq809nkdkvi6ndc94jr8wjk9d6wyzbbkr2"))

(define rust-tokio-1.48.0
  (crate-source "tokio" "1.48.0"
                "0244qva5pksy8gam6llf7bd6wbk2vkab9lx26yyf08dix810wdpz"))

(define rust-tokio-macros-2.4.0
  (crate-source "tokio-macros" "2.4.0"
                "0lnpg14h1v3fh2jvnc8cz7cjf0m7z1xgkwfpcyy632g829imjgb9"))

(define rust-tokio-native-tls-0.3.1
  (crate-source "tokio-native-tls" "0.3.1"
                "1wkfg6zn85zckmv4im7mv20ca6b1vmlib5xwz9p7g19wjfmpdbmv"))

(define rust-tokio-rustls-0.26.4
  (crate-source "tokio-rustls" "0.26.4"
                "0qggwknz9w4bbsv1z158hlnpkm97j3w8v31586jipn99byaala8p"))

(define rust-tokio-util-0.7.17
  (crate-source "tokio-util" "0.7.17"
                "152m2rp40bjphca5j581csczarvvr974zvwpzpldcwv0wygi9yif"))

(define rust-toml-0.9.8
  (crate-source "toml" "0.9.8"
                "1n569s0dgdmqjy21wf85df7kx3vb1zgin3pc2rvy4j8lnqgqpp7h"))

(define rust-toml-datetime-0.6.8
  (crate-source "toml_datetime" "0.6.8"
                "0hgv7v9g35d7y9r2afic58jvlwnf73vgd1mz2k8gihlgrf73bmqd"))

(define rust-toml-datetime-0.7.3
  (crate-source "toml_datetime" "0.7.3"
                "0cs5f8y4rdsmmwipjclmq97lrwppjy2qa3vja4f9d5xwxcwvdkgj"))

(define rust-toml-datetime-0.7.5+spec-1.1.0
  (crate-source "toml_datetime" "0.7.5+spec-1.1.0"
                "0iqkgvgsxmszpai53dbip7sf2igic39s4dby29dbqf1h9bnwzqcj"))

(define rust-toml-edit-0.22.22
  (crate-source "toml_edit" "0.22.22"
                "1xf7sxfzmnc45f75x302qrn5aph52vc8w226v59yhrm211i8vr2a"))

(define rust-toml-edit-0.23.10+spec-1.0.0
  (crate-source "toml_edit" "0.23.10+spec-1.0.0"
                "0saj5c676j8a3sqaj9akkp09wambg8aflji4zblwwa70azvvkj44"))

(define rust-toml-parser-1.0.4
  (crate-source "toml_parser" "1.0.4"
                "03l0750d1cyliij9vac4afpp1syh1a6yhbbalnslpnsvsdlf5jy0"))

(define rust-toml-parser-1.0.6+spec-1.1.0
  (crate-source "toml_parser" "1.0.6+spec-1.1.0"
                "0i5zxv5y3z9g6r3gm6ly4q0hhkahh013q4rys2fz04cf195qn6d3"))

(define rust-toml-writer-1.0.4
  (crate-source "toml_writer" "1.0.4"
                "1wkvcdy1ymp2qfipmb74fv3xa7m7qz7ps9hndllasx1nfda2p2yz"))

(define rust-tower-0.5.2
  (crate-source "tower" "0.5.2"
                "1ybmd59nm4abl9bsvy6rx31m4zvzp5rja2slzpn712y9b68ssffh"))

(define rust-tower-http-0.6.8
  (crate-source "tower-http" "0.6.8"
                "1y514jwzbyrmrkbaajpwmss4rg0mak82k16d6588w9ncaffmbrnl"))

(define rust-tower-layer-0.3.3
  (crate-source "tower-layer" "0.3.3"
                "03kq92fdzxin51w8iqix06dcfgydyvx7yr6izjq0p626v9n2l70j"))

(define rust-tower-service-0.3.3
  (crate-source "tower-service" "0.3.3"
                "1hzfkvkci33ra94xjx64vv3pp0sq346w06fpkcdwjcid7zhvdycd"))

(define rust-tracing-0.1.41
  (crate-source "tracing" "0.1.41"
                "1l5xrzyjfyayrwhvhldfnwdyligi1mpqm8mzbi2m1d6y6p2hlkkq"))

(define rust-tracing-0.1.43
  (crate-source "tracing" "0.1.43"
                "0iy6dyqk9ign880xw52snixrs507hj2xqyflaa4kf6aw1c5dj59d"))

(define rust-tracing-0.1.44
  (crate-source "tracing" "0.1.44"
                "006ilqkg1lmfdh3xhg3z762izfwmxcvz0w7m4qx2qajbz9i1drv3"))

(define rust-tracing-attributes-0.1.28
  (crate-source "tracing-attributes" "0.1.28"
                "0v92l9cxs42rdm4m5hsa8z7ln1xsiw1zc2iil8c6k7lzq0jf2nir"))

(define rust-tracing-attributes-0.1.31
  (crate-source "tracing-attributes" "0.1.31"
                "1np8d77shfvz0n7camx2bsf1qw0zg331lra0hxb4cdwnxjjwz43l"))

(define rust-tracing-core-0.1.33
  (crate-source "tracing-core" "0.1.33"
                "170gc7cxyjx824r9kr17zc9gvzx89ypqfdzq259pr56gg5bwjwp6"))

(define rust-tracing-core-0.1.35
  (crate-source "tracing-core" "0.1.35"
                "0v0az9hivci6bysd796za7g823gkasb8qmdqdsiwd2awmd7y413s"))

(define rust-tracing-core-0.1.36
  (crate-source "tracing-core" "0.1.36"
                "16mpbz6p8vd6j7sf925k9k8wzvm9vdfsjbynbmaxxyq6v7wwm5yv"))

(define rust-try-lock-0.2.5
  (crate-source "try-lock" "0.2.5"
                "0jqijrrvm1pyq34zn1jmy2vihd4jcrjlvsh4alkjahhssjnsn8g4"))

(define rust-ttf-parser-0.25.1
  (crate-source "ttf-parser" "0.25.1"
                "0cbgqglcwwjg3hirwq6xlza54w04mb5x02kf7zx4hrw50xmr1pyj"))

(define rust-typenum-1.19.0
  (crate-source "typenum" "1.19.0"
                "1fw2mpbn2vmqan56j1b3fbpcdg80mz26fm53fs16bq5xcq84hban"))

(define rust-udev-0.9.1
  (crate-source "udev" "0.9.1"
                "10dxy4vd16mkq0xi24d3nwgiv007qhrmry493j9nj5szp6bw3mg3"))

(define rust-uds-windows-1.1.0
  (crate-source "uds_windows" "1.1.0"
                "1fb4y65pw0rsp0gyfyinjazlzxz1f6zv7j4zmb20l5pxwv1ypnl9"))

(define rust-unicode-bidi-0.3.18
  (crate-source "unicode-bidi" "0.3.18"
                "1xcxwbsqa24b8vfchhzyyzgj0l6bn51ib5v8j6krha0m77dva72w"))

(define rust-unicode-bidi-mirroring-0.4.0
  (crate-source "unicode-bidi-mirroring" "0.4.0"
                "1zirs1z3ahlwy7swg7apnm3pc6vix1g15q0kn6fx8rmvc266xyjx"))

(define rust-unicode-ccc-0.4.0
  (crate-source "unicode-ccc" "0.4.0"
                "0gjhxwx27ywm3rcbb0m5q20w8zxi51440b3ps6swi6ywpj4d8qff"))

(define rust-unicode-ident-1.0.12
  (crate-source "unicode-ident" "1.0.12"
                "0jzf1znfpb2gx8nr8mvmyqs1crnv79l57nxnbiszc7xf7ynbjm1k"))

(define rust-unicode-ident-1.0.14
  (crate-source "unicode-ident" "1.0.14"
                "10ywa1pg0glgkr4l3dppjxizr9r2b7im0ycbfa0137l69z5fdfdd"))

(define rust-unicode-ident-1.0.18
  (crate-source "unicode-ident" "1.0.18"
                "04k5r6sijkafzljykdq26mhjpmhdx4jwzvn1lh90g9ax9903jpss"))

(define rust-unicode-ident-1.0.22
  (crate-source "unicode-ident" "1.0.22"
                "1x8xrz17vqi6qmkkcqr8cyf0an76ig7390j9cnqnk47zyv2gf4lk"))

(define rust-unicode-ident-1.0.23
  (crate-source "unicode-ident" "1.0.23"
                "17m0v64a09qyp1b4jy2d9ywinp1ic2rd2jyxcjdvwy4qm0wd0zak"))

(define rust-unicode-linebreak-0.1.5
  (crate-source "unicode-linebreak" "0.1.5"
                "07spj2hh3daajg335m4wdav6nfkl0f6c0q72lc37blr97hych29v"))

(define rust-unicode-properties-0.1.4
  (crate-source "unicode-properties" "0.1.4"
                "07fpm3sqq7lm9gmgpxa93z31q933h3c3ypfwy4cdh6l42g3miw3x"))

(define rust-unicode-script-0.5.8
  (crate-source "unicode-script" "0.5.8"
                "1vmifpgd0map3frmvhszhl96k82crcry083prv05wii7p45x8fiq"))

(define rust-unicode-segmentation-1.12.0
  (crate-source "unicode-segmentation" "1.12.0"
                "14qla2jfx74yyb9ds3d2mpwpa4l4lzb9z57c6d2ba511458z5k7n"))

(define rust-unicode-vo-0.1.0
  (crate-source "unicode-vo" "0.1.0"
                "151sha088v9jyfvbg5164xh4dk72g53b82xm4zzbf5dlagzqdlxi"))

(define rust-unicode-width-0.2.2
  (crate-source "unicode-width" "0.2.2"
                "0m7jjzlcccw716dy9423xxh0clys8pfpllc5smvfxrzdf66h9b5l"))

(define rust-unicode-xid-0.2.6
  (crate-source "unicode-xid" "0.2.6"
                "0lzqaky89fq0bcrh6jj6bhlz37scfd8c7dsj5dq7y32if56c1hgb"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"))

(define rust-url-2.5.7
  (crate-source "url" "2.5.7"
                "0nzghdv0kcksyvri0npxbjzyx2ihprks5k590y77bld355m17g08"))

(define rust-usvg-0.45.1
  (crate-source "usvg" "0.45.1"
                "1vs7gfhyxjgkgibgwfjniwrszfw0iivj1aq06hq8nfxfzc39pgl0"))

(define rust-utf8-iter-1.0.4
  (crate-source "utf8_iter" "1.0.4"
                "1gmna9flnj8dbyd8ba17zigrp9c4c3zclngf5lnb5yvz1ri41hdn"))

(define rust-utf8parse-0.2.1
  (crate-source "utf8parse" "0.2.1"
                "02ip1a0az0qmc2786vxk2nqwsgcwf17d3a38fkf0q7hrmwh9c6vi"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-uuid-1.20.0
  (crate-source "uuid" "1.20.0"
                "0vwpi7vnwjsfcx58nfks9sgmsz4wpbsk06qlwhgxf34v265x6j7f"))

(define rust-vcpkg-0.2.15
  (crate-source "vcpkg" "0.2.15"
                "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))

(define rust-want-0.3.1
  (crate-source "want" "0.3.1"
                "03hbfrnvqqdchb5kgxyavb9jabwza0dmh2vw5kg0dq8rxl57d9xz"))

(define rust-wasi-0.11.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.0+wasi-snapshot-preview1"
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))

(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))

(define rust-wasip2-1.0.1+wasi-0.2.4
  (crate-source "wasip2" "1.0.1+wasi-0.2.4"
                "1rsqmpspwy0zja82xx7kbkbg9fv34a4a2if3sbd76dy64a244qh5"))

(define rust-wasip2-1.0.2+wasi-0.2.9
  (crate-source "wasip2" "1.0.2+wasi-0.2.9"
                "1xdw7v08jpfjdg94sp4lbdgzwa587m5ifpz6fpdnkh02kwizj5wm"))

(define rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
  (crate-source "wasip3" "0.4.0+wasi-0.3.0-rc-2026-01-06"
                "19dc8p0y2mfrvgk3qw3c3240nfbylv22mvyxz84dqpgai2zzha2l"))

(define rust-wasm-bindgen-0.2.106
  (crate-source "wasm-bindgen" "0.2.106"
                "1zc0pcyv0w1dhp8r7ybmmfjsf4g18q784h0k7mv2sjm67x1ryx8d"))

(define rust-wasm-bindgen-0.2.108
  (crate-source "wasm-bindgen" "0.2.108"
                "0rl5pn80sdhj2p2r28lp3k50a8mpppzgwzssz2f3jdqyxhq4l0k4"))

(define rust-wasm-bindgen-futures-0.4.56
  (crate-source "wasm-bindgen-futures" "0.4.56"
                "0z6f0zkylpgbfb7dkh7a85dxdwm57q7c2np2bngfxzh4sqi9cvc3"))

(define rust-wasm-bindgen-futures-0.4.58
  (crate-source "wasm-bindgen-futures" "0.4.58"
                "0vqywn9df5i6mms3sw47v3kj7rzx8ryghqq0xb4jk05fs1zyg9kh"))

(define rust-wasm-bindgen-macro-0.2.106
  (crate-source "wasm-bindgen-macro" "0.2.106"
                "1czfwzhqrkzyyhd3g58mdwb2jjk4q2pl9m1fajyfvfpq70k0vjs8"))

(define rust-wasm-bindgen-macro-0.2.108
  (crate-source "wasm-bindgen-macro" "0.2.108"
                "026nnvakp0w6j3ghpcxn31shj9wx8bv8x7nk3gkk40klkjfj72q0"))

(define rust-wasm-bindgen-macro-support-0.2.106
  (crate-source "wasm-bindgen-macro-support" "0.2.106"
                "0h6ddq6cc6jf9phsdh2a3x8lpjhmkya86ihfz3fdk4jzrpamkyyf"))

(define rust-wasm-bindgen-macro-support-0.2.108
  (crate-source "wasm-bindgen-macro-support" "0.2.108"
                "0m9sj475ypgifbkvksjsqs2gy3bq96f87ychch784m4gspiblmjj"))

(define rust-wasm-bindgen-shared-0.2.106
  (crate-source "wasm-bindgen-shared" "0.2.106"
                "1d0dh3jn77qz67n5zh0s3rvzlbjv926p0blq5bvng2v4gq2kiifb"))

(define rust-wasm-bindgen-shared-0.2.108
  (crate-source "wasm-bindgen-shared" "0.2.108"
                "04ix7v99rvj5730553j58pqsrwpf9sqazr60y3cchx5cr60ba08z"))

(define rust-wasm-encoder-0.244.0
  (crate-source "wasm-encoder" "0.244.0"
                "06c35kv4h42vk3k51xjz1x6hn3mqwfswycmr6ziky033zvr6a04r"))

(define rust-wasm-metadata-0.244.0
  (crate-source "wasm-metadata" "0.244.0"
                "02f9dhlnryd2l7zf03whlxai5sv26x4spfibjdvc3g9gd8z3a3mv"))

(define rust-wasmparser-0.244.0
  (crate-source "wasmparser" "0.244.0"
                "1zi821hrlsxfhn39nqpmgzc0wk7ax3dv6vrs5cw6kb0v5v3hgf27"))

(define rust-wasmtimer-0.4.3
  (crate-source "wasmtimer" "0.4.3"
                "0jv45z8yvgn6jgqqhmd1rpakk0yhf13gr5s6hhskw0gak5mqsn8w"))

(define rust-wayland-backend-0.1.2
  (crate-source "wayland-backend" "0.1.2"
                "1n1yi6vna23wfkrpk1j46sx5qbsijh50viha4sra73by8lkqxd21"))

(define rust-wayland-backend-0.3.12
  (crate-source "wayland-backend" "0.3.12"
                "1yb4s5mbcis3z3gcmxq2lzgrcw2li7jsfr9ayi4gcsyrrja43rpy"))

(define rust-wayland-client-0.30.2
  (crate-source "wayland-client" "0.30.2"
                "1j3as2g1znrs2lpkksqcvx8pag85yiwwbcv6wb3lyrqgfxa9d728"))

(define rust-wayland-client-0.31.12
  (crate-source "wayland-client" "0.31.12"
                "1v1b2b2s0ld41psn3v2p3c6i590iz3r427czrf3c3dpv6yjzmrmq"))

(define rust-wayland-csd-frame-0.3.0
  (crate-source "wayland-csd-frame" "0.3.0"
                "0zjcmcqprfzx57hlm741n89ssp4sha5yh5cnmbk2agflvclm0p32"))

(define rust-wayland-cursor-0.31.12
  (crate-source "wayland-cursor" "0.31.12"
                "0y5hzl8z0da7sa2fg877bwl5mi56k15askmpx2qhcjq6nssw8r2q"))

(define rust-wayland-protocols-0.30.1
  (crate-source "wayland-protocols" "0.30.1"
                "0kcvvli38gdjb9c7dpa2s0ix4nnqfq7n2bbc39370kx9bhg10a1v"))

(define rust-wayland-protocols-0.32.10
  (crate-source "wayland-protocols" "0.32.10"
                "1wzl7ly3ahi2y4swf8wmlqaj3gck4fpmwf6ymbfxd37wpkzskvds"))

(define rust-wayland-protocols-experimental-20250721.0.1
  (crate-source "wayland-protocols-experimental" "20250721.0.1"
                "1cfbimd2qbbcgv21i3l7kq3pm6lvrjbb7d6pj33sxjld29izi8a0"))

(define rust-wayland-protocols-misc-0.3.10
  (crate-source "wayland-protocols-misc" "0.3.10"
                "0kn3nk770vf29d227d8r88rpzr6i4x9q3pb9f6inlh65xvymh73r"))

(define rust-wayland-protocols-plasma-0.3.10
  (crate-source "wayland-protocols-plasma" "0.3.10"
                "0srh0xhf4jpz8s1kx531lh0ixzx5v6p3iwwpk9d562ih3536765a"))

(define rust-wayland-protocols-wlr-0.3.10
  (crate-source "wayland-protocols-wlr" "0.3.10"
                "1ws5fd7qs5vf3digbnn20n7mks2sdg76sy13b36k836g0bgpqng9"))

(define rust-wayland-scanner-0.30.1
  (crate-source "wayland-scanner" "0.30.1"
                "03ikmfwacsgbym2y4raf05knl1qjlgg81sy0174jxhzvayr77f5r"))

(define rust-wayland-scanner-0.31.8
  (crate-source "wayland-scanner" "0.31.8"
                "1qw971z9jcxdw8s371gx2anmwb95m59y38q3k11qxrk3d95yj8sl"))

(define rust-wayland-sys-0.30.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "wayland-sys" "0.30.1"
                "01man4ll2kyxp9x2934rhnf98522pzwsd2c6jwr73q08qqma1cln"))

(define rust-wayland-sys-0.31.8
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "wayland-sys" "0.31.8"
                "1zdxrcl8paklwir0lag1i80k6h0iq1f80d925b4p9yaymk1vyv8y"))

(define rust-web-sys-0.3.83
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.83"
                "1b1pw450ig62xr0cy1wfjlbahvmi725jl64d150j0hacfy6q4clv"))

(define rust-web-sys-0.3.85
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "web-sys" "0.3.85"
                "1645c202gyw21m6kxw4ya81vrapl40hlb8m9iqhjj8fra7jk4bii"))

(define rust-web-time-1.1.0
  (crate-source "web-time" "1.1.0"
                "1fx05yqx83dhx628wb70fyy10yjfq1jpl20qfqhdkymi13rq0ras"))

(define rust-weezl-0.1.12
  (crate-source "weezl" "0.1.12"
                "122a1dhha6cib5az4ihcqlh60ns2bi6rskdv875p94lbvj6wk2m2"))

(define rust-wgpu-27.0.1
  (crate-source "wgpu" "27.0.1"
                "0xvsrmcp862zh3sd6nkr0cbsznna4c3l1hrilzkms4nygjn8prmz"))

(define rust-wgpu-core-27.0.3
  (crate-source "wgpu-core" "27.0.3"
                "1rya571frb73y32iwnhnxada31dk64vzj6b1nabihfsl2pjmv9r7"))

(define rust-wgpu-core-deps-apple-27.0.0
  (crate-source "wgpu-core-deps-apple" "27.0.0"
                "0cyjas0gx6wxjshv3f25p7gvqycnl7ckyphxaqlwgq4visaswwh7"))

(define rust-wgpu-core-deps-emscripten-27.0.0
  (crate-source "wgpu-core-deps-emscripten" "27.0.0"
                "1dd69pd9z8m4a88h7qmkcjr20qmnfhkxn7fqrwz83c4m992c6smh"))

(define rust-wgpu-core-deps-windows-linux-android-27.0.0
  (crate-source "wgpu-core-deps-windows-linux-android" "27.0.0"
                "1hq3ka3mm507fysc3y017hp19bdj8a95mw1086778w8ssqkp06bi"))

(define rust-wgpu-hal-27.0.4
  (crate-source "wgpu-hal" "27.0.4"
                "1khk8gb81izrd9815fdy4ywq1fsczynswwgz9ay9iqbyqmhwn8av"))

(define rust-wgpu-types-27.0.1
  (crate-source "wgpu-types" "27.0.1"
                "1sx5blmyas6k40ilhlrfsy31xcvcf0l1dn9dgxrxp42r756gip5g"))

(define rust-which-6.0.1
  (crate-source "which" "6.0.1"
                "1mz0vijj9qvsmfqkjqw3wf8zqn19p2x0gg7gzfnhaa1bibsy84c2"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-util-0.1.11
  (crate-source "winapi-util" "0.1.11"
                "08hdl7mkll7pz8whg869h58c1r9y7in0w0pk8fm24qc77k0b39y2"))

(define rust-winapi-util-0.1.6
  (crate-source "winapi-util" "0.1.6"
                "15i5lm39wd44004i9d5qspry2cynkrpvwzghr6s2c3dsk28nz7pj"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-window-clipboard-0.5.1
  (crate-source "window_clipboard" "0.5.1"
                "0cyph2xr8g9rx24cnh5xrg75v3njc4l4iyski3g2vbsy60k44rfm"))

(define rust-windows-0.58.0
  (crate-source "windows" "0.58.0"
                "1dkjj94b0gn91nn1n22cvm4afsj98f5qrhcl3112v6f4jcfx816x"))

(define rust-windows-0.62.2
  (crate-source "windows" "0.62.2"
                "10457l9ihrbw8j79z2v4plyjxkf6xvb5npd0lqwmkh702gpaszsj"))

(define rust-windows-aarch64-gnullvm-0.42.2
  (crate-source "windows_aarch64_gnullvm" "0.42.2"
                "1y4q0qmvl0lvp7syxvfykafvmwal5hrjb4fmv04bqs0bawc52yjr"))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"))

(define rust-windows-aarch64-gnullvm-0.52.0
  (crate-source "windows_aarch64_gnullvm" "0.52.0"
                "1shmn1kbdc0bpphcxz0vlph96bxz0h1jlmh93s9agf2dbpin8xyb"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-gnullvm-0.53.0
  (crate-source "windows_aarch64_gnullvm" "0.53.0"
                "0r77pbpbcf8bq4yfwpz2hpq3vns8m0yacpvs2i5cn6fx1pwxbf46"))

(define rust-windows-aarch64-gnullvm-0.53.1
  (crate-source "windows_aarch64_gnullvm" "0.53.1"
                "0lqvdm510mka9w26vmga7hbkmrw9glzc90l4gya5qbxlm1pl3n59"))

(define rust-windows-aarch64-msvc-0.42.2
  (crate-source "windows_aarch64_msvc" "0.42.2"
                "0hsdikjl5sa1fva5qskpwlxzpc5q9l909fpl1w6yy1hglrj8i3p0"))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"))

(define rust-windows-aarch64-msvc-0.52.0
  (crate-source "windows_aarch64_msvc" "0.52.0"
                "1vvmy1ypvzdvxn9yf0b8ygfl85gl2gpcyvsvqppsmlpisil07amv"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-aarch64-msvc-0.53.0
  (crate-source "windows_aarch64_msvc" "0.53.0"
                "0v766yqw51pzxxwp203yqy39ijgjamp54hhdbsyqq6x1c8gilrf7"))

(define rust-windows-aarch64-msvc-0.53.1
  (crate-source "windows_aarch64_msvc" "0.53.1"
                "01jh2adlwx043rji888b22whx4bm8alrk3khjpik5xn20kl85mxr"))

(define rust-windows-collections-0.3.2
  (crate-source "windows-collections" "0.3.2"
                "0436rjbkqn3j9m2v2lcmwwk0l3n2r57yvqb7fcy4m8d8y5ddkci3"))

(define rust-windows-core-0.58.0
  (crate-source "windows-core" "0.58.0"
                "16czypy425jzmiys4yb3pwsh7cm6grxn9kjp889iqnf2r17d99kb"))

(define rust-windows-core-0.62.2
  (crate-source "windows-core" "0.62.2"
                "1swxpv1a8qvn3bkxv8cn663238h2jccq35ff3nsj61jdsca3ms5q"))

(define rust-windows-future-0.3.2
  (crate-source "windows-future" "0.3.2"
                "1jq5qs2dwzf6rl60f8gr49z2mifxsrdh4y4yfdws467ya41gkmp1"))

(define rust-windows-i686-gnu-0.42.2
  (crate-source "windows_i686_gnu" "0.42.2"
                "0kx866dfrby88lqs9v1vgmrkk1z6af9lhaghh5maj7d4imyr47f6"))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"))

(define rust-windows-i686-gnu-0.52.0
  (crate-source "windows_i686_gnu" "0.52.0"
                "04zkglz4p3pjsns5gbz85v4s5aw102raz4spj4b0lmm33z5kg1m2"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnu-0.53.0
  (crate-source "windows_i686_gnu" "0.53.0"
                "1hvjc8nv95sx5vdd79fivn8bpm7i517dqyf4yvsqgwrmkmjngp61"))

(define rust-windows-i686-gnu-0.53.1
  (crate-source "windows_i686_gnu" "0.53.1"
                "18wkcm82ldyg4figcsidzwbg1pqd49jpm98crfz0j7nqd6h6s3ln"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-gnullvm-0.53.0
  (crate-source "windows_i686_gnullvm" "0.53.0"
                "04df1in2k91qyf1wzizvh560bvyzq20yf68k8xa66vdzxnywrrlw"))

(define rust-windows-i686-gnullvm-0.53.1
  (crate-source "windows_i686_gnullvm" "0.53.1"
                "030qaxqc4salz6l4immfb6sykc6gmhyir9wzn2w8mxj8038mjwzs"))

(define rust-windows-i686-msvc-0.42.2
  (crate-source "windows_i686_msvc" "0.42.2"
                "0q0h9m2aq1pygc199pa5jgc952qhcnf0zn688454i7v4xjv41n24"))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"))

(define rust-windows-i686-msvc-0.52.0
  (crate-source "windows_i686_msvc" "0.52.0"
                "16kvmbvx0vr0zbgnaz6nsks9ycvfh5xp05bjrhq65kj623iyirgz"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

(define rust-windows-i686-msvc-0.53.0
  (crate-source "windows_i686_msvc" "0.53.0"
                "0pcvb25fkvqnp91z25qr5x61wyya12lx8p7nsa137cbb82ayw7sq"))

(define rust-windows-i686-msvc-0.53.1
  (crate-source "windows_i686_msvc" "0.53.1"
                "1hi6scw3mn2pbdl30ji5i4y8vvspb9b66l98kkz350pig58wfyhy"))

(define rust-windows-implement-0.58.0
  (crate-source "windows-implement" "0.58.0"
                "16spr5z65z21qyv379rv2mb1s5q2i9ibd1p2pkn0dr9qr535pg9b"))

(define rust-windows-implement-0.60.2
  (crate-source "windows-implement" "0.60.2"
                "1psxhmklzcf3wjs4b8qb42qb6znvc142cb5pa74rsyxm1822wgh5"))

(define rust-windows-interface-0.58.0
  (crate-source "windows-interface" "0.58.0"
                "059mxmfvx3x88q74ms0qlxmj2pnidmr5mzn60hakn7f95m34qg05"))

(define rust-windows-interface-0.59.3
  (crate-source "windows-interface" "0.59.3"
                "0n73cwrn4247d0axrk7gjp08p34x1723483jxjxjdfkh4m56qc9z"))

(define rust-windows-link-0.1.3
  (crate-source "windows-link" "0.1.3"
                "12kr1p46dbhpijr4zbwr2spfgq8i8c5x55mvvfmyl96m01cx4sjy"))

(define rust-windows-link-0.2.1
  (crate-source "windows-link" "0.2.1"
                "1rag186yfr3xx7piv5rg8b6im2dwcf8zldiflvb22xbzwli5507h"))

(define rust-windows-numerics-0.3.1
  (crate-source "windows-numerics" "0.3.1"
                "09hgbg8pf89r4090yyhh9q29ppi7yyxkgmga9ascshy19a240bkf"))

(define rust-windows-registry-0.6.1
  (crate-source "windows-registry" "0.6.1"
                "082p7l615qk8a4g8g15yipc5lghga6cgfhm74wm7zknwzgvjnx82"))

(define rust-windows-result-0.2.0
  (crate-source "windows-result" "0.2.0"
                "03mf2z1xcy2slhhsm15z24p76qxgm2m74xdjp8bihyag47c4640x"))

(define rust-windows-result-0.4.1
  (crate-source "windows-result" "0.4.1"
                "1d9yhmrmmfqh56zlj751s5wfm9a2aa7az9rd7nn5027nxa4zm0bp"))

(define rust-windows-strings-0.1.0
  (crate-source "windows-strings" "0.1.0"
                "042dxvi3133f7dyi2pgcvknwkikk47k8bddwxbq5s0l6qhjv3nac"))

(define rust-windows-strings-0.5.1
  (crate-source "windows-strings" "0.5.1"
                "14bhng9jqv4fyl7lqjz3az7vzh8pw0w4am49fsqgcz67d67x0dvq"))

(define rust-windows-sys-0.45.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.45.0"
                "1l36bcqm4g89pknfp8r9rl1w4bn017q6a8qlx8viv0xjxzjkna3m"))

(define rust-windows-sys-0.48.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.52.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-sys-0.60.2
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.60.2"
                "1jrbc615ihqnhjhxplr2kw7rasrskv9wj3lr80hgfd42sbj01xgj"))

(define rust-windows-sys-0.61.2
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.61.2"
                "1z7k3y9b6b5h52kid57lvmvm05362zv1v8w0gc7xyv5xphlp44xf"))

(define rust-windows-targets-0.42.2
  (crate-source "windows-targets" "0.42.2"
                "0wfhnib2fisxlx8c507dbmh97kgij4r6kcxdi0f9nk6l1k080lcf"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.0
  (crate-source "windows-targets" "0.52.0"
                "1kg7a27ynzw8zz3krdgy6w5gbqcji27j1sz4p7xk2j5j8082064a"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-targets-0.53.3
  (crate-source "windows-targets" "0.53.3"
                "14fwwm136dhs3i1impqrrip7nvkra3bdxa4nqkblj604qhqn1znm"))

(define rust-windows-targets-0.53.5
  (crate-source "windows-targets" "0.53.5"
                "1wv9j2gv3l6wj3gkw5j1kr6ymb5q6dfc42yvydjhv3mqa7szjia9"))

(define rust-windows-threading-0.2.1
  (crate-source "windows-threading" "0.2.1"
                "0dsvsy33vxs0153z4n39sqkzx382cjjkrd46rb3z3zfak5dvsj9r"))

(define rust-windows-x86-64-gnu-0.42.2
  (crate-source "windows_x86_64_gnu" "0.42.2"
                "0dnbf2xnp3xrvy8v9mgs3var4zq9v9yh9kv79035rdgyp2w15scd"))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"))

(define rust-windows-x86-64-gnu-0.52.0
  (crate-source "windows_x86_64_gnu" "0.52.0"
                "1zdy4qn178sil5sdm63lm7f0kkcjg6gvdwmcprd2yjmwn8ns6vrx"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnu-0.53.0
  (crate-source "windows_x86_64_gnu" "0.53.0"
                "1flh84xkssn1n6m1riddipydcksp2pdl45vdf70jygx3ksnbam9f"))

(define rust-windows-x86-64-gnu-0.53.1
  (crate-source "windows_x86_64_gnu" "0.53.1"
                "16d4yiysmfdlsrghndr97y57gh3kljkwhfdbcs05m1jasz6l4f4w"))

(define rust-windows-x86-64-gnullvm-0.42.2
  (crate-source "windows_x86_64_gnullvm" "0.42.2"
                "18wl9r8qbsl475j39zvawlidp1bsbinliwfymr43fibdld31pm16"))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"))

(define rust-windows-x86-64-gnullvm-0.52.0
  (crate-source "windows_x86_64_gnullvm" "0.52.0"
                "17lllq4l2k1lqgcnw1cccphxp9vs7inq99kjlm2lfl9zklg7wr8s"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-gnullvm-0.53.0
  (crate-source "windows_x86_64_gnullvm" "0.53.0"
                "0mvc8119xpbi3q2m6mrjcdzl6afx4wffacp13v76g4jrs1fh6vha"))

(define rust-windows-x86-64-gnullvm-0.53.1
  (crate-source "windows_x86_64_gnullvm" "0.53.1"
                "1qbspgv4g3q0vygkg8rnql5c6z3caqv38japiynyivh75ng1gyhg"))

(define rust-windows-x86-64-msvc-0.42.2
  (crate-source "windows_x86_64_msvc" "0.42.2"
                "1w5r0q0yzx827d10dpjza2ww0j8iajqhmb54s735hhaj66imvv4s"))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"))

(define rust-windows-x86-64-msvc-0.52.0
  (crate-source "windows_x86_64_msvc" "0.52.0"
                "012wfq37f18c09ij5m6rniw7xxn5fcvrxbqd0wd8vgnl3hfn9yfz"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-windows-x86-64-msvc-0.53.0
  (crate-source "windows_x86_64_msvc" "0.53.0"
                "11h4i28hq0zlnjcaqi2xdxr7ibnpa8djfggch9rki1zzb8qi8517"))

(define rust-windows-x86-64-msvc-0.53.1
  (crate-source "windows_x86_64_msvc" "0.53.1"
                "0l6npq76vlq4ksn4bwsncpr8508mk0gmznm6wnhjg95d19gzzfyn"))

(define rust-winit-0.30.12
  (crate-source "winit" "0.30.12"
                "0cn7wvli4s0l3v5rf6s3rn4j72mdc5p2sxhz6bv0jh4wssg4nvf6"))

(define rust-winnow-0.6.20
  (crate-source "winnow" "0.6.20"
                "16y4i8z9vh8hazjxg5mvmq0c5i35wlk8rxi5gkq6cn5vlb0zxh9n"))

(define rust-winnow-0.7.14
  (crate-source "winnow" "0.7.14"
                "0a88ahjqhyn2ln1yplq2xsigm09kxqkdkkk2c2mfxkbzszln8lss"))

(define rust-winnow-0.7.2
  (crate-source "winnow" "0.7.2"
                "00znis68117jk13aw41g048wvvv3h0xw5jmhlg8rh8cg2vm0ssar"))

(define rust-winsafe-0.0.19
  (crate-source "winsafe" "0.0.19"
                "0169xy9mjma8dys4m8v4x0xhw2gkbhv2v1wsbvcjl9bhnxxd2dfi"))

(define rust-wit-bindgen-0.46.0
  (crate-source "wit-bindgen" "0.46.0"
                "0ngysw50gp2wrrfxbwgp6dhw1g6sckknsn3wm7l00vaf7n48aypi"))

(define rust-wit-bindgen-0.51.0
  (crate-source "wit-bindgen" "0.51.0"
                "19fazgch8sq5cvjv3ynhhfh5d5x08jq2pkw8jfb05vbcyqcr496p"))

(define rust-wit-bindgen-core-0.51.0
  (crate-source "wit-bindgen-core" "0.51.0"
                "1p2jszqsqbx8k7y8nwvxg65wqzxjm048ba5phaq8r9iy9ildwqga"))

(define rust-wit-bindgen-rust-0.51.0
  (crate-source "wit-bindgen-rust" "0.51.0"
                "08bzn5fsvkb9x9wyvyx98qglknj2075xk1n7c5jxv15jykh6didp"))

(define rust-wit-bindgen-rust-macro-0.51.0
  (crate-source "wit-bindgen-rust-macro" "0.51.0"
                "0ymizapzv2id89igxsz2n587y2hlfypf6n8kyp68x976fzyrn3qc"))

(define rust-wit-component-0.244.0
  (crate-source "wit-component" "0.244.0"
                "1clwxgsgdns3zj2fqnrjcp8y5gazwfa1k0sy5cbk0fsmx4hflrlx"))

(define rust-wit-parser-0.244.0
  (crate-source "wit-parser" "0.244.0"
                "0dm7avvdxryxd5b02l0g5h6933z1cw5z0d4wynvq2cywq55srj7c"))

(define rust-writeable-0.6.2
  (crate-source "writeable" "0.6.2"
                "1fg08y97n6vk7l0rnjggw3xyrii6dcqg54wqaxldrlk98zdy1pcy"))

(define rust-wyz-0.5.1
  (crate-source "wyz" "0.5.1"
                "1vdrfy7i2bznnzjdl9vvrzljvs4s3qm8bnlgqwln6a941gy61wq5"))

(define rust-x11-dl-2.21.0
  (crate-source "x11-dl" "2.21.0"
                "0vsiq62xpcfm0kn9zjw5c9iycvccxl22jya8wnk18lyxzqj5jwrq"))

(define rust-x11rb-0.13.2
  (crate-source "x11rb" "0.13.2"
                "053lvnaw9ycbl791mgwly2hw27q6vqgzrb1y5kz1as52wmdsm4wr"))

(define rust-x11rb-protocol-0.13.2
  (crate-source "x11rb-protocol" "0.13.2"
                "1g81cznbyn522b0fbis0i44wh3adad2vhsz5pzf99waf3sbc4vza"))

(define rust-xcursor-0.3.10
  (crate-source "xcursor" "0.3.10"
                "0awgy98awg4ydcfmynqfcwvl4bnnfcm4i2vvnk2n926a02jy9jdy"))

(define rust-xdg-home-1.3.0
  (crate-source "xdg-home" "1.3.0"
                "1xm122zz0wjc8p8cmchij0j9nw34hwncb39jc7dc0mgvb2rdl77c"))

(define rust-xkbcommon-dl-0.4.2
  (crate-source "xkbcommon-dl" "0.4.2"
                "1iai0r3b5skd9vbr8z5b0qixiz8jblzfm778ddm8ba596a0dwffh"))

(define rust-xkeysym-0.2.1
  (crate-source "xkeysym" "0.2.1"
                "0mksx670cszyd7jln6s7dhkw11hdfv7blwwr3isq98k22ljh1k5r"))

(define rust-xml-rs-0.8.28
  (crate-source "xml-rs" "0.8.28"
                "0grdj7xwbki5zrkalrg8dljyf14y4yj3wrj34sbzqp06i9zk7s1s"))

(define rust-xmlwriter-0.1.0
  (crate-source "xmlwriter" "0.1.0"
                "1fg0ldmkgiis6hnxpi1c9gy7v23y0lpi824bp8yp12fi3r82lypc"))

(define rust-yansi-1.0.1
  (crate-source "yansi" "1.0.1"
                "0jdh55jyv0dpd38ij4qh60zglbw9aa8wafqai6m0wa7xaxk3mrfg"))

(define rust-yazi-0.2.1
  (crate-source "yazi" "0.2.1"
                "1dgv0dd0329dcbs57z2wj03daz8y1axgprw3pf5yf5jsbcjkh5z0"))

(define rust-yoke-0.8.1
  (crate-source "yoke" "0.8.1"
                "0m29dm0bf5iakxgma0bj6dbmc3b8qi9b1vaw9sa76kdqmz3fbmkj"))

(define rust-yoke-derive-0.8.1
  (crate-source "yoke-derive" "0.8.1"
                "0pbyja133jnng4mrhimzdq4a0y26421g734ybgz8wsgbfhl0andn"))

(define rust-zbus-5.13.2
  (crate-source "zbus" "5.13.2"
                "1ldxqkwy577n7w5ss3lshg9adpyji3vvllj61jr3xahagaczzzhv"))

(define rust-zbus-5.5.0
  (crate-source "zbus" "5.5.0"
                "0dmjaih7gi2d0fa37zzylvbmxqn80x4d7haxr5xn86za93v37hsr"))

(define rust-zbus-macros-5.13.2
  (crate-source "zbus_macros" "5.13.2"
                "1wa6z2gzpzna0mww9jj9db9cq573g914ix6y2ddyxzp8vf85mg8b"))

(define rust-zbus-macros-5.5.0
  (crate-source "zbus_macros" "5.5.0"
                "1h4zf0wh647fvv97bnsr3ah64cgcnz1r8d10c2q3w2hdxc8as9gk"))

(define rust-zbus-names-4.1.0
  (crate-source "zbus_names" "4.1.0"
                "0yv4rqd0hrky4iqb6gzmkwwwibncigpcwmkqsipq8w8zh4w7lsw5"))

(define rust-zbus-names-4.3.1
  (crate-source "zbus_names" "4.3.1"
                "03y5f8xwzmk4y5wb4g95a1hl48mxlmhcbwqz62mrnqbqbdnszn7z"))

(define rust-zeno-0.3.3
  (crate-source "zeno" "0.3.3"
                "0915lg3b1qiixizic9kwj2bw42x35baxqvl9dn0m2plkj91drwvd"))

(define rust-zerocopy-0.8.39
  (crate-source "zerocopy" "0.8.39"
                "0jmf1iqns5sq07k3dscsgyc706pycar67rrq4j9nrnzacgb3avfv"))

(define rust-zerocopy-derive-0.8.39
  (crate-source "zerocopy-derive" "0.8.39"
                "05z5yfq0mx3xdqadrgq5sd4d03nl82d9r0vp1qchaip9d4qws8j1"))

(define rust-zerofrom-0.1.6
  (crate-source "zerofrom" "0.1.6"
                "19dyky67zkjichsb7ykhv0aqws3q0jfvzww76l66c19y6gh45k2h"))

(define rust-zerofrom-derive-0.1.6
  (crate-source "zerofrom-derive" "0.1.6"
                "00l5niw7c1b0lf1vhvajpjmcnbdp2vn96jg4nmkhq2db0rp5s7np"))

(define rust-zeroize-1.8.2
  (crate-source "zeroize" "1.8.2"
                "1l48zxgcv34d7kjskr610zqsm6j2b4fcr2vfh9jm9j1jgvk58wdr"))

(define rust-zeroize-derive-1.4.2
  (crate-source "zeroize_derive" "1.4.2"
                "0sczjlqjdmrp3wn62g7mw6p438c9j4jgp2f9zamd56991mdycdnf"))

(define rust-zerotrie-0.2.3
  (crate-source "zerotrie" "0.2.3"
                "0lbqznlqazmrwwzslw0ci7p3pqxykrbfhq29npj0gmb2amxc2n9a"))

(define rust-zerovec-0.11.5
  (crate-source "zerovec" "0.11.5"
                "00m0p47k2g9mkv505hky5xh3r6ps7v8qc0dy4pspg542jj972a3c"))

(define rust-zerovec-derive-0.11.2
  (crate-source "zerovec-derive" "0.11.2"
                "1wsig4h5j7a1scd5hrlnragnazjny9qjc44hancb6p6a76ay7p7a"))

(define rust-zip-6.0.0
  (crate-source "zip" "6.0.0"
                "12qn4kxpvgqs07z5hfzpj1cp1njczgvwjxl5n04nrpkgqg3haapb"))

(define rust-zlib-rs-0.5.4
  (crate-source "zlib-rs" "0.5.4"
                "0dajfzya2fn24ixvz34xvnjqb8l3nl1ivblg2shy2yv79l23dyai"))

(define rust-zmij-1.0.20
  (crate-source "zmij" "1.0.20"
                "1xzsih7rddwdqicd0wbaqjvag70caqyhgl1lx17fyzsvbpx8vsad"))

(define rust-zopfli-0.8.3
  (crate-source "zopfli" "0.8.3"
                "0jaj5dyh3mks0805h4ldrsh5pwq4i2jc9dc9zwjm91k3gmwxhp7h"))

(define rust-zstd-0.13.3
  (crate-source "zstd" "0.13.3"
                "12n0h4w9l526li7jl972rxpyf012jw3nwmji2qbjghv9ll8y67p9"))

(define rust-zstd-safe-7.2.4
  (crate-source "zstd-safe" "7.2.4"
                "179vxmkzhpz6cq6mfzvgwc99bpgllkr6lwxq7ylh5dmby3aw8jcg"))

(define rust-zstd-sys-2.0.16+zstd.1.5.7
  ;; TODO: Check bundled sources.
  (crate-source "zstd-sys" "2.0.16+zstd.1.5.7"
                "0j1pd2iaqpvaxlgqmmijj68wma7xwdv9grrr63j873yw5ay9xqci"))

(define rust-zune-core-0.4.12
  (crate-source "zune-core" "0.4.12"
                "0jj1ra86klzlcj9aha9als9d1dzs7pqv3azs1j3n96822wn3lhiz"))

(define rust-zune-jpeg-0.4.21
  (crate-source "zune-jpeg" "0.4.21"
                "04r7g6y9jp7d4c9bq23rz3gwzlr1dsl7vdk4yly35bc4jf52rki9"))

(define rust-zvariant-5.1.0
  (crate-source "zvariant" "5.1.0"
                "0vx5w4a2s21y9agr1y9lbm8maj3rli4sjmg42aiybw9jmkk0w851"))

(define rust-zvariant-5.9.2
  (crate-source "zvariant" "5.9.2"
                "1i1jn8lvsj79lnfyw21lrsimg2jj0gfj6w6wglrm2y8cyks4xdk8"))

(define rust-zvariant-derive-5.1.0
  (crate-source "zvariant_derive" "5.1.0"
                "05l9m8ypcf28v3xjwkl7zc2bz6hlgp976v6kpd7i1jg6zabknzk8"))

(define rust-zvariant-derive-5.9.2
  (crate-source "zvariant_derive" "5.9.2"
                "0p21bv2kzphhcc71597ya3b0m8hr6wyw2adrqqnbbbxpbsbmska8"))

(define rust-zvariant-utils-3.2.0
  (crate-source "zvariant_utils" "3.2.0"
                "0d7wllndiv7vwgapmji3q9sxmzbaqfdxjwkqnx9vbmz58gpdyvp1"))

(define rust-zvariant-utils-3.3.0
  (crate-source "zvariant_utils" "3.3.0"
                "1sf5i71in36gc08jhak83pprnkam8gk936cqlq9hzx7q9sk26p7p"))

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
                                         rust-winsafe-0.0.19))
                     (extest =>
                             (list rust-autocfg-1.5.0
                                   rust-bitflags-1.3.2
                                   rust-bitflags-2.9.4
                                   rust-bitvec-1.0.1
                                   rust-cc-1.2.35
                                   rust-cfg-if-1.0.3
                                   rust-cfg-aliases-0.2.1
                                   rust-dlib-0.5.2
                                   rust-downcast-rs-1.2.1
                                   rust-evdev-0.13.1
                                   rust-find-msvc-tools-0.1.0
                                   rust-funty-2.0.0
                                   rust-hermit-abi-0.3.9
                                   rust-io-lifetimes-1.0.11
                                   rust-libc-0.2.175
                                   rust-libloading-0.8.8
                                   rust-log-0.4.27
                                   rust-memchr-2.7.5
                                   rust-memoffset-0.7.1
                                   rust-nix-0.26.4
                                   rust-nix-0.29.0
                                   rust-once-cell-1.21.3
                                   rust-pkg-config-0.3.32
                                   rust-proc-macro2-1.0.101
                                   rust-quick-xml-0.28.2
                                   rust-quote-1.0.40
                                   rust-radium-0.7.0
                                   rust-scoped-tls-1.0.1
                                   rust-shlex-1.3.0
                                   rust-smallvec-1.15.1
                                   rust-tap-1.0.1
                                   rust-unicode-ident-1.0.18
                                   rust-wayland-backend-0.1.2
                                   rust-wayland-client-0.30.2
                                   rust-wayland-protocols-0.30.1
                                   rust-wayland-scanner-0.30.1
                                   rust-wayland-sys-0.30.1
                                   rust-windows-link-0.1.3
                                   rust-windows-sys-0.48.0
                                   rust-windows-targets-0.48.5
                                   rust-windows-targets-0.53.3
                                   rust-windows-aarch64-gnullvm-0.48.5
                                   rust-windows-aarch64-gnullvm-0.53.0
                                   rust-windows-aarch64-msvc-0.48.5
                                   rust-windows-aarch64-msvc-0.53.0
                                   rust-windows-i686-gnu-0.48.5
                                   rust-windows-i686-gnu-0.53.0
                                   rust-windows-i686-gnullvm-0.53.0
                                   rust-windows-i686-msvc-0.48.5
                                   rust-windows-i686-msvc-0.53.0
                                   rust-windows-x86-64-gnu-0.48.5
                                   rust-windows-x86-64-gnu-0.53.0
                                   rust-windows-x86-64-gnullvm-0.48.5
                                   rust-windows-x86-64-gnullvm-0.53.0
                                   rust-windows-x86-64-msvc-0.48.5
                                   rust-windows-x86-64-msvc-0.53.0
                                   rust-wyz-0.5.1))
                     (mdrop =>
                            (list rust-ab-glyph-0.2.32
                             rust-ab-glyph-rasterizer-0.1.10
                             rust-adler2-2.0.1
                             rust-ahash-0.8.12
                             rust-aho-corasick-1.1.4
                             rust-android-activity-0.6.0
                             rust-android-build-0.1.3
                             rust-android-properties-0.2.2
                             rust-android-system-properties-0.1.5
                             rust-anstream-0.6.21
                             rust-anstyle-1.0.13
                             rust-anstyle-parse-0.2.7
                             rust-anstyle-query-1.1.5
                             rust-anstyle-wincon-3.0.11
                             rust-anyhow-1.0.101
                             rust-arrayref-0.3.9
                             rust-arrayvec-0.7.6
                             rust-as-raw-xcb-connection-1.0.1
                             rust-ash-0.38.0+1.3.281
                             rust-async-broadcast-0.7.2
                             rust-async-channel-2.5.0
                             rust-async-executor-1.13.3
                             rust-async-io-2.6.0
                             rust-async-lock-3.4.2
                             rust-async-process-2.5.0
                             rust-async-recursion-1.1.1
                             rust-async-signal-0.2.13
                             rust-async-task-4.7.1
                             rust-async-trait-0.1.89
                             rust-atomic-waker-1.1.2
                             rust-autocfg-1.5.0
                             rust-base64-0.22.1
                             rust-bit-set-0.8.0
                             rust-bit-vec-0.8.0
                             rust-bitflags-1.3.2
                             rust-bitflags-2.10.0
                             rust-block-0.1.6
                             rust-block2-0.5.1
                             rust-block2-0.6.2
                             rust-blocking-1.6.2
                             rust-bumpalo-3.19.1
                             rust-bytecount-0.6.9
                             rust-bytemuck-1.25.0
                             rust-bytemuck-derive-1.10.2
                             rust-byteorder-lite-0.1.0
                             rust-bytes-1.11.1
                             rust-calloop-0.13.0
                             rust-calloop-0.14.3
                             rust-calloop-wayland-source-0.3.0
                             rust-calloop-wayland-source-0.4.1
                             rust-cc-1.2.55
                             rust-cesu8-1.1.0
                             rust-cfg-if-1.0.4
                             rust-cfg-aliases-0.2.1
                             rust-clap-4.5.57
                             rust-clap-builder-4.5.57
                             rust-clap-derive-4.5.55
                             rust-clap-lex-0.7.7
                             rust-clipboard-win-5.4.1
                             rust-clipboard-macos-0.1.1
                             rust-clipboard-wayland-0.2.2
                             rust-clipboard-x11-0.4.3
                             rust-codespan-reporting-0.12.0
                             rust-color-quant-1.1.0
                             rust-colorchoice-1.0.4
                             rust-combine-4.6.7
                             rust-concurrent-queue-2.5.0
                             rust-core-foundation-0.9.4
                             rust-core-foundation-0.10.1
                             rust-core-foundation-sys-0.8.7
                             rust-core-graphics-0.23.2
                             rust-core-graphics-types-0.1.3
                             rust-core-graphics-types-0.2.0
                             rust-core-maths-0.1.1
                             rust-cosmic-text-0.15.0
                             rust-crc32fast-1.5.0
                             rust-crossbeam-utils-0.8.21
                             rust-crunchy-0.2.4
                             rust-cryoglyph-0.1.0
                             rust-ctor-lite-0.1.1
                             rust-cursor-icon-1.2.0
                             rust-data-url-0.3.2
                             rust-dispatch-0.2.0
                             rust-dispatch2-0.3.0
                             rust-dlib-0.5.2
                             rust-document-features-0.2.12
                             rust-downcast-rs-1.2.1
                             rust-dpi-0.1.2
                             rust-endi-1.1.1
                             rust-enumflags2-0.7.12
                             rust-enumflags2-derive-0.7.12
                             rust-env-filter-0.1.4
                             rust-env-logger-0.11.8
                             rust-equivalent-1.0.2
                             rust-errno-0.3.14
                             rust-error-code-3.3.2
                             rust-etagere-0.2.15
                             rust-euclid-0.22.13
                             rust-event-listener-5.4.1
                             rust-event-listener-strategy-0.5.4
                             rust-fastrand-2.3.0
                             rust-fdeflate-0.3.7
                             rust-find-msvc-tools-0.1.9
                             rust-flate2-1.1.9
                             rust-float-cmp-0.9.0
                             rust-fnv-1.0.7
                             rust-foldhash-0.1.5
                             rust-foldhash-0.2.0
                             rust-font-types-0.10.1
                             rust-fontconfig-parser-0.5.8
                             rust-fontdb-0.23.0
                             rust-foreign-types-0.5.0
                             rust-foreign-types-macros-0.2.3
                             rust-foreign-types-shared-0.3.1
                             rust-futures-0.3.31
                             rust-futures-channel-0.3.31
                             rust-futures-core-0.3.31
                             rust-futures-executor-0.3.31
                             rust-futures-io-0.3.31
                             rust-futures-lite-2.6.1
                             rust-futures-macro-0.3.31
                             rust-futures-sink-0.3.31
                             rust-futures-task-0.3.31
                             rust-futures-util-0.3.31
                             rust-gethostname-1.1.0
                             rust-getrandom-0.3.4
                             rust-getrandom-0.4.1
                             rust-gif-0.13.3
                             rust-gl-generator-0.14.0
                             rust-glam-0.25.0
                             rust-glow-0.16.0
                             rust-glutin-wgl-sys-0.6.1
                             rust-gpu-alloc-0.6.0
                             rust-gpu-alloc-types-0.3.0
                             rust-gpu-allocator-0.27.0
                             rust-gpu-descriptor-0.3.2
                             rust-gpu-descriptor-types-0.2.0
                             rust-guillotiere-0.6.2
                             rust-half-2.7.1
                             rust-harfrust-0.3.2
                             rust-hashbrown-0.15.5
                             rust-hashbrown-0.16.1
                             rust-heck-0.5.0
                             rust-hermit-abi-0.5.2
                             rust-hex-0.4.3
                             rust-hexf-parse-0.2.1
                             rust-iced-0.14.0
                             rust-iced-core-0.14.0
                             rust-iced-debug-0.14.0
                             rust-iced-futures-0.14.0
                             rust-iced-graphics-0.14.0
                             rust-iced-program-0.14.0
                             rust-iced-renderer-0.14.0
                             rust-iced-runtime-0.14.0
                             rust-iced-tiny-skia-0.14.0
                             rust-iced-wgpu-0.14.0
                             rust-iced-widget-0.14.2
                             rust-iced-winit-0.14.0
                             rust-id-arena-2.3.0
                             rust-image-webp-0.2.4
                             rust-imagesize-0.13.0
                             rust-indexmap-2.13.0
                             rust-io-kit-sys-0.4.1
                             rust-is-terminal-polyfill-1.70.2
                             rust-itoa-1.0.17
                             rust-jiff-0.2.19
                             rust-jiff-static-0.2.19
                             rust-jni-0.21.1
                             rust-jni-sys-0.3.0
                             rust-jobserver-0.1.34
                             rust-js-sys-0.3.85
                             rust-khronos-egl-6.0.0
                             rust-khronos-api-3.1.0
                             rust-kurbo-0.10.4
                             rust-kurbo-0.11.3
                             rust-leb128fmt-0.1.0
                             rust-libc-0.2.181
                             rust-libloading-0.8.9
                             rust-libm-0.2.16
                             rust-libredox-0.1.12
                             rust-lilt-0.8.1
                             rust-linebender-resource-handle-0.1.1
                             rust-linux-raw-sys-0.4.15
                             rust-linux-raw-sys-0.9.4
                             rust-linux-raw-sys-0.11.0
                             rust-litrs-1.0.0
                             rust-lock-api-0.4.14
                             rust-log-0.4.29
                             rust-lru-0.16.3
                             rust-mach2-0.4.3
                             rust-malloc-buf-0.0.6
                             rust-memchr-2.8.0
                             rust-memmap2-0.9.9
                             rust-memoffset-0.9.1
                             rust-metal-0.32.0
                             rust-miniz-oxide-0.8.9
                             rust-mundy-0.2.2
                             rust-naga-27.0.3
                             rust-ndk-0.9.0
                             rust-ndk-context-0.1.1
                             rust-ndk-sys-0.6.0+11769913
                             rust-num-traits-0.2.19
                             rust-num-cpus-1.17.0
                             rust-num-enum-0.7.5
                             rust-num-enum-derive-0.7.5
                             rust-nusb-0.2.1
                             rust-objc-0.2.7
                             rust-objc-sys-0.3.5
                             rust-objc2-0.5.2
                             rust-objc2-0.6.3
                             rust-objc2-app-kit-0.2.2
                             rust-objc2-app-kit-0.3.2
                             rust-objc2-cloud-kit-0.2.2
                             rust-objc2-cloud-kit-0.3.2
                             rust-objc2-contacts-0.2.2
                             rust-objc2-core-data-0.2.2
                             rust-objc2-core-data-0.3.2
                             rust-objc2-core-foundation-0.3.2
                             rust-objc2-core-graphics-0.3.2
                             rust-objc2-core-image-0.2.2
                             rust-objc2-core-image-0.3.2
                             rust-objc2-core-location-0.2.2
                             rust-objc2-core-text-0.3.2
                             rust-objc2-core-video-0.3.2
                             rust-objc2-encode-4.1.0
                             rust-objc2-foundation-0.2.2
                             rust-objc2-foundation-0.3.2
                             rust-objc2-io-surface-0.3.2
                             rust-objc2-link-presentation-0.2.2
                             rust-objc2-metal-0.2.2
                             rust-objc2-quartz-core-0.2.2
                             rust-objc2-quartz-core-0.3.2
                             rust-objc2-symbols-0.2.2
                             rust-objc2-ui-kit-0.2.2
                             rust-objc2-uniform-type-identifiers-0.2.2
                             rust-objc2-user-notifications-0.2.2
                             rust-once-cell-1.21.3
                             rust-once-cell-polyfill-1.70.2
                             rust-orbclient-0.3.50
                             rust-ordered-float-4.6.0
                             rust-ordered-stream-0.2.0
                             rust-owned-ttf-parser-0.25.1
                             rust-papergrid-0.17.0
                             rust-parking-2.2.1
                             rust-parking-lot-0.12.5
                             rust-parking-lot-core-0.9.12
                             rust-paste-1.0.15
                             rust-percent-encoding-2.3.2
                             rust-pico-args-0.5.0
                             rust-pin-project-1.1.10
                             rust-pin-project-internal-1.1.10
                             rust-pin-project-lite-0.2.16
                             rust-pin-utils-0.1.0
                             rust-piper-0.2.4
                             rust-pkg-config-0.3.32
                             rust-png-0.17.16
                             rust-polling-3.11.0
                             rust-portable-atomic-1.13.1
                             rust-portable-atomic-util-0.2.5
                             rust-presser-0.3.1
                             rust-prettyplease-0.2.37
                             rust-proc-macro-crate-3.4.0
                             rust-proc-macro-error-attr2-2.0.0
                             rust-proc-macro-error2-2.0.1
                             rust-proc-macro2-1.0.106
                             rust-profiling-1.0.17
                             rust-quick-error-2.0.1
                             rust-quick-xml-0.38.4
                             rust-quote-1.0.44
                             rust-r-efi-5.3.0
                             rust-range-alloc-0.1.4
                             rust-rangemap-1.7.1
                             rust-raw-window-handle-0.6.2
                             rust-read-fonts-0.35.0
                             rust-redox-syscall-0.4.1
                             rust-redox-syscall-0.5.18
                             rust-redox-syscall-0.7.0
                             rust-regex-1.12.3
                             rust-regex-automata-0.4.14
                             rust-regex-syntax-0.8.9
                             rust-renderdoc-sys-1.1.0
                             rust-resvg-0.45.1
                             rust-rgb-0.8.52
                             rust-roxmltree-0.20.0
                             rust-rustc-hash-1.1.0
                             rust-rustc-hash-2.1.1
                             rust-rustix-0.38.44
                             rust-rustix-1.1.3
                             rust-rustversion-1.0.22
                             rust-rustybuzz-0.20.1
                             rust-same-file-1.0.6
                             rust-scoped-tls-1.0.1
                             rust-scopeguard-1.2.0
                             rust-sctk-adwaita-0.10.1
                             rust-self-cell-1.2.2
                             rust-semver-1.0.27
                             rust-serde-1.0.228
                             rust-serde-core-1.0.228
                             rust-serde-derive-1.0.228
                             rust-serde-json-1.0.149
                             rust-serde-repr-0.1.20
                             rust-shlex-1.3.0
                             rust-signal-hook-registry-1.4.8
                             rust-simd-adler32-0.3.8
                             rust-simplecss-0.2.2
                             rust-siphasher-1.0.2
                             rust-skrifa-0.37.0
                             rust-slab-0.4.12
                             rust-slotmap-1.1.1
                             rust-smallvec-1.15.1
                             rust-smithay-client-toolkit-0.19.2
                             rust-smithay-client-toolkit-0.20.0
                             rust-smithay-clipboard-0.7.3
                             rust-smol-str-0.2.2
                             rust-softbuffer-0.4.8
                             rust-spirv-0.3.0+sdk-1.3.268.0
                             rust-static-assertions-1.1.0
                             rust-strict-num-0.1.1
                             rust-strsim-0.11.1
                             rust-svg-fmt-0.4.5
                             rust-svgtypes-0.15.3
                             rust-swash-0.2.6
                             rust-syn-2.0.114
                             rust-sys-locale-0.3.2
                             rust-tabled-0.20.0
                             rust-tabled-derive-0.11.0
                             rust-tempfile-3.25.0
                             rust-termcolor-1.4.1
                             rust-testing-table-0.3.0
                             rust-thiserror-1.0.69
                             rust-thiserror-2.0.18
                             rust-thiserror-impl-1.0.69
                             rust-thiserror-impl-2.0.18
                             rust-tiny-skia-0.11.4
                             rust-tiny-skia-path-0.11.4
                             rust-tiny-xlib-0.2.4
                             rust-tinyvec-1.10.0
                             rust-tinyvec-macros-0.1.1
                             rust-toml-datetime-0.7.5+spec-1.1.0
                             rust-toml-edit-0.23.10+spec-1.0.0
                             rust-toml-parser-1.0.6+spec-1.1.0
                             rust-tracing-0.1.44
                             rust-tracing-attributes-0.1.31
                             rust-tracing-core-0.1.36
                             rust-ttf-parser-0.25.1
                             rust-uds-windows-1.1.0
                             rust-unicode-bidi-0.3.18
                             rust-unicode-bidi-mirroring-0.4.0
                             rust-unicode-ccc-0.4.0
                             rust-unicode-ident-1.0.23
                             rust-unicode-linebreak-0.1.5
                             rust-unicode-properties-0.1.4
                             rust-unicode-script-0.5.8
                             rust-unicode-segmentation-1.12.0
                             rust-unicode-vo-0.1.0
                             rust-unicode-width-0.2.2
                             rust-unicode-xid-0.2.6
                             rust-usvg-0.45.1
                             rust-utf8parse-0.2.2
                             rust-uuid-1.20.0
                             rust-version-check-0.9.5
                             rust-walkdir-2.5.0
                             rust-wasip2-1.0.2+wasi-0.2.9
                             rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
                             rust-wasm-bindgen-0.2.108
                             rust-wasm-bindgen-futures-0.4.58
                             rust-wasm-bindgen-macro-0.2.108
                             rust-wasm-bindgen-macro-support-0.2.108
                             rust-wasm-bindgen-shared-0.2.108
                             rust-wasm-encoder-0.244.0
                             rust-wasm-metadata-0.244.0
                             rust-wasmparser-0.244.0
                             rust-wasmtimer-0.4.3
                             rust-wayland-backend-0.3.12
                             rust-wayland-client-0.31.12
                             rust-wayland-csd-frame-0.3.0
                             rust-wayland-cursor-0.31.12
                             rust-wayland-protocols-0.32.10
                             rust-wayland-protocols-experimental-20250721.0.1
                             rust-wayland-protocols-misc-0.3.10
                             rust-wayland-protocols-plasma-0.3.10
                             rust-wayland-protocols-wlr-0.3.10
                             rust-wayland-scanner-0.31.8
                             rust-wayland-sys-0.31.8
                             rust-web-sys-0.3.85
                             rust-web-time-1.1.0
                             rust-weezl-0.1.12
                             rust-wgpu-27.0.1
                             rust-wgpu-core-27.0.3
                             rust-wgpu-core-deps-apple-27.0.0
                             rust-wgpu-core-deps-emscripten-27.0.0
                             rust-wgpu-core-deps-windows-linux-android-27.0.0
                             rust-wgpu-hal-27.0.4
                             rust-wgpu-types-27.0.1
                             rust-winapi-0.3.9
                             rust-winapi-i686-pc-windows-gnu-0.4.0
                             rust-winapi-util-0.1.11
                             rust-winapi-x86-64-pc-windows-gnu-0.4.0
                             rust-window-clipboard-0.5.1
                             rust-windows-0.58.0
                             rust-windows-0.62.2
                             rust-windows-collections-0.3.2
                             rust-windows-core-0.58.0
                             rust-windows-core-0.62.2
                             rust-windows-future-0.3.2
                             rust-windows-implement-0.58.0
                             rust-windows-implement-0.60.2
                             rust-windows-interface-0.58.0
                             rust-windows-interface-0.59.3
                             rust-windows-link-0.2.1
                             rust-windows-numerics-0.3.1
                             rust-windows-result-0.2.0
                             rust-windows-result-0.4.1
                             rust-windows-strings-0.1.0
                             rust-windows-strings-0.5.1
                             rust-windows-sys-0.45.0
                             rust-windows-sys-0.52.0
                             rust-windows-sys-0.59.0
                             rust-windows-sys-0.60.2
                             rust-windows-sys-0.61.2
                             rust-windows-targets-0.42.2
                             rust-windows-targets-0.52.6
                             rust-windows-targets-0.53.5
                             rust-windows-threading-0.2.1
                             rust-windows-aarch64-gnullvm-0.42.2
                             rust-windows-aarch64-gnullvm-0.52.6
                             rust-windows-aarch64-gnullvm-0.53.1
                             rust-windows-aarch64-msvc-0.42.2
                             rust-windows-aarch64-msvc-0.52.6
                             rust-windows-aarch64-msvc-0.53.1
                             rust-windows-i686-gnu-0.42.2
                             rust-windows-i686-gnu-0.52.6
                             rust-windows-i686-gnu-0.53.1
                             rust-windows-i686-gnullvm-0.52.6
                             rust-windows-i686-gnullvm-0.53.1
                             rust-windows-i686-msvc-0.42.2
                             rust-windows-i686-msvc-0.52.6
                             rust-windows-i686-msvc-0.53.1
                             rust-windows-x86-64-gnu-0.42.2
                             rust-windows-x86-64-gnu-0.52.6
                             rust-windows-x86-64-gnu-0.53.1
                             rust-windows-x86-64-gnullvm-0.42.2
                             rust-windows-x86-64-gnullvm-0.52.6
                             rust-windows-x86-64-gnullvm-0.53.1
                             rust-windows-x86-64-msvc-0.42.2
                             rust-windows-x86-64-msvc-0.52.6
                             rust-windows-x86-64-msvc-0.53.1
                             rust-winit-0.30.12
                             rust-winnow-0.7.14
                             rust-wit-bindgen-0.51.0
                             rust-wit-bindgen-core-0.51.0
                             rust-wit-bindgen-rust-0.51.0
                             rust-wit-bindgen-rust-macro-0.51.0
                             rust-wit-component-0.244.0
                             rust-wit-parser-0.244.0
                             rust-x11-dl-2.21.0
                             rust-x11rb-0.13.2
                             rust-x11rb-protocol-0.13.2
                             rust-xcursor-0.3.10
                             rust-xkbcommon-dl-0.4.2
                             rust-xkeysym-0.2.1
                             rust-xml-rs-0.8.28
                             rust-xmlwriter-0.1.0
                             rust-yazi-0.2.1
                             rust-zbus-5.13.2
                             rust-zbus-macros-5.13.2
                             rust-zbus-names-4.3.1
                             rust-zeno-0.3.3
                             rust-zerocopy-0.8.39
                             rust-zerocopy-derive-0.8.39
                             rust-zmij-1.0.20
                             rust-zune-core-0.4.12
                             rust-zune-jpeg-0.4.21
                             rust-zvariant-5.9.2
                             rust-zvariant-derive-5.9.2
                             rust-zvariant-utils-3.3.0))
                     (mod-installer =>
                                    (list rust-adler2-2.0.1
                                     rust-aes-0.8.4
                                     rust-aho-corasick-1.1.4
                                     rust-anstream-0.6.21
                                     rust-anstyle-1.0.13
                                     rust-anstyle-parse-0.2.7
                                     rust-anstyle-query-1.1.5
                                     rust-anstyle-wincon-3.0.11
                                     rust-arbitrary-1.4.2
                                     rust-atomic-waker-1.1.2
                                     rust-base64-0.22.1
                                     rust-bitflags-2.10.0
                                     rust-block-buffer-0.10.4
                                     rust-bumpalo-3.19.0
                                     rust-bytes-1.11.0
                                     rust-bzip2-0.6.1
                                     rust-cc-1.2.49
                                     rust-cfg-if-1.0.4
                                     rust-cipher-0.4.4
                                     rust-clap-4.5.53
                                     rust-clap-builder-4.5.53
                                     rust-clap-derive-4.5.49
                                     rust-clap-lex-0.7.6
                                     rust-colorchoice-1.0.4
                                     rust-confy-2.0.0
                                     rust-constant-time-eq-0.3.1
                                     rust-core-foundation-0.9.4
                                     rust-core-foundation-sys-0.8.7
                                     rust-cpufeatures-0.2.17
                                     rust-crc-3.4.0
                                     rust-crc-catalog-2.4.0
                                     rust-crc32fast-1.5.0
                                     rust-crypto-common-0.1.7
                                     rust-deflate64-0.1.10
                                     rust-deranged-0.5.5
                                     rust-derive-arbitrary-1.4.2
                                     rust-diff-0.1.13
                                     rust-digest-0.10.7
                                     rust-displaydoc-0.2.5
                                     rust-encoding-rs-0.8.35
                                     rust-env-filter-0.1.4
                                     rust-env-logger-0.11.8
                                     rust-equivalent-1.0.2
                                     rust-errno-0.3.14
                                     rust-etcetera-0.10.0
                                     rust-fastrand-2.3.0
                                     rust-find-msvc-tools-0.1.5
                                     rust-flate2-1.1.5
                                     rust-fnv-1.0.7
                                     rust-foreign-types-0.3.2
                                     rust-foreign-types-shared-0.1.1
                                     rust-form-urlencoded-1.2.2
                                     rust-futures-channel-0.3.31
                                     rust-futures-core-0.3.31
                                     rust-futures-io-0.3.31
                                     rust-futures-sink-0.3.31
                                     rust-futures-task-0.3.31
                                     rust-futures-util-0.3.31
                                     rust-generic-array-0.14.7
                                     rust-getrandom-0.2.16
                                     rust-getrandom-0.3.4
                                     rust-h2-0.4.12
                                     rust-hashbrown-0.16.1
                                     rust-heck-0.5.0
                                     rust-hmac-0.12.1
                                     rust-home-0.5.12
                                     rust-http-1.4.0
                                     rust-http-body-1.0.1
                                     rust-http-body-util-0.1.3
                                     rust-httparse-1.10.1
                                     rust-hyper-1.8.1
                                     rust-hyper-rustls-0.27.7
                                     rust-hyper-tls-0.6.0
                                     rust-hyper-util-0.1.19
                                     rust-icu-collections-2.1.1
                                     rust-icu-locale-core-2.1.1
                                     rust-icu-normalizer-2.1.1
                                     rust-icu-normalizer-data-2.1.1
                                     rust-icu-properties-2.1.2
                                     rust-icu-properties-data-2.1.2
                                     rust-icu-provider-2.1.1
                                     rust-idna-1.1.0
                                     rust-idna-adapter-1.2.1
                                     rust-indexmap-2.12.1
                                     rust-inout-0.1.4
                                     rust-ipnet-2.11.0
                                     rust-iri-string-0.7.9
                                     rust-is-terminal-polyfill-1.70.2
                                     rust-itoa-1.0.15
                                     rust-jiff-0.2.16
                                     rust-jiff-static-0.2.16
                                     rust-jobserver-0.1.34
                                     rust-js-sys-0.3.83
                                     rust-lazy-static-1.5.0
                                     rust-libbz2-rs-sys-0.2.2
                                     rust-libc-0.2.178
                                     rust-libz-rs-sys-0.5.4
                                     rust-linux-raw-sys-0.11.0
                                     rust-litemap-0.8.1
                                     rust-log-0.4.29
                                     rust-lzma-rust2-0.13.0
                                     rust-memchr-2.7.6
                                     rust-mime-0.3.17
                                     rust-miniz-oxide-0.8.9
                                     rust-mio-1.1.1
                                     rust-native-tls-0.2.14
                                     rust-num-conv-0.1.0
                                     rust-once-cell-1.21.3
                                     rust-once-cell-polyfill-1.70.2
                                     rust-openssl-0.10.75
                                     rust-openssl-macros-0.1.1
                                     rust-openssl-probe-0.1.6
                                     rust-openssl-sys-0.9.111
                                     rust-pbkdf2-0.12.2
                                     rust-percent-encoding-2.3.2
                                     rust-pin-project-lite-0.2.16
                                     rust-pin-utils-0.1.0
                                     rust-pkg-config-0.3.32
                                     rust-portable-atomic-1.11.1
                                     rust-portable-atomic-util-0.2.4
                                     rust-potential-utf-0.1.4
                                     rust-powerfmt-0.2.0
                                     rust-ppmd-rust-1.3.0
                                     rust-pretty-assertions-1.4.1
                                     rust-proc-macro2-1.0.103
                                     rust-quote-1.0.42
                                     rust-r-efi-5.3.0
                                     rust-regex-1.12.2
                                     rust-regex-automata-0.4.13
                                     rust-regex-syntax-0.8.8
                                     rust-reqwest-0.12.25
                                     rust-ring-0.17.14
                                     rust-rustix-1.1.2
                                     rust-rustls-0.23.35
                                     rust-rustls-pki-types-1.13.1
                                     rust-rustls-webpki-0.103.8
                                     rust-rustversion-1.0.22
                                     rust-ryu-1.0.20
                                     rust-same-file-1.0.6
                                     rust-schannel-0.1.28
                                     rust-security-framework-2.11.1
                                     rust-security-framework-sys-2.15.0
                                     rust-serde-1.0.228
                                     rust-serde-core-1.0.228
                                     rust-serde-derive-1.0.228
                                     rust-serde-json-1.0.145
                                     rust-serde-spanned-1.0.3
                                     rust-serde-urlencoded-0.7.1
                                     rust-sha1-0.10.6
                                     rust-sha2-0.10.9
                                     rust-shlex-1.3.0
                                     rust-simd-adler32-0.3.8
                                     rust-slab-0.4.11
                                     rust-smallvec-1.15.1
                                     rust-socket2-0.6.1
                                     rust-stable-deref-trait-1.2.1
                                     rust-strsim-0.11.1
                                     rust-subtle-2.6.1
                                     rust-syn-2.0.111
                                     rust-sync-wrapper-1.0.2
                                     rust-synstructure-0.13.2
                                     rust-system-configuration-0.6.1
                                     rust-system-configuration-sys-0.6.0
                                     rust-tempfile-3.23.0
                                     rust-thiserror-2.0.17
                                     rust-thiserror-impl-2.0.17
                                     rust-time-0.3.44
                                     rust-time-core-0.1.6
                                     rust-tinystr-0.8.2
                                     rust-tokio-1.48.0
                                     rust-tokio-native-tls-0.3.1
                                     rust-tokio-rustls-0.26.4
                                     rust-tokio-util-0.7.17
                                     rust-toml-0.9.8
                                     rust-toml-datetime-0.7.3
                                     rust-toml-parser-1.0.4
                                     rust-toml-writer-1.0.4
                                     rust-tower-0.5.2
                                     rust-tower-http-0.6.8
                                     rust-tower-layer-0.3.3
                                     rust-tower-service-0.3.3
                                     rust-tracing-0.1.43
                                     rust-tracing-core-0.1.35
                                     rust-try-lock-0.2.5
                                     rust-typenum-1.19.0
                                     rust-unicode-ident-1.0.22
                                     rust-untrusted-0.9.0
                                     rust-url-2.5.7
                                     rust-utf8-iter-1.0.4
                                     rust-utf8parse-0.2.2
                                     rust-vcpkg-0.2.15
                                     rust-version-check-0.9.5
                                     rust-walkdir-2.5.0
                                     rust-want-0.3.1
                                     rust-wasi-0.11.1+wasi-snapshot-preview1
                                     rust-wasip2-1.0.1+wasi-0.2.4
                                     rust-wasm-bindgen-0.2.106
                                     rust-wasm-bindgen-futures-0.4.56
                                     rust-wasm-bindgen-macro-0.2.106
                                     rust-wasm-bindgen-macro-support-0.2.106
                                     rust-wasm-bindgen-shared-0.2.106
                                     rust-web-sys-0.3.83
                                     rust-winapi-util-0.1.11
                                     rust-windows-link-0.2.1
                                     rust-windows-registry-0.6.1
                                     rust-windows-result-0.4.1
                                     rust-windows-strings-0.5.1
                                     rust-windows-sys-0.52.0
                                     rust-windows-sys-0.59.0
                                     rust-windows-sys-0.60.2
                                     rust-windows-sys-0.61.2
                                     rust-windows-targets-0.52.6
                                     rust-windows-targets-0.53.5
                                     rust-windows-aarch64-gnullvm-0.52.6
                                     rust-windows-aarch64-gnullvm-0.53.1
                                     rust-windows-aarch64-msvc-0.52.6
                                     rust-windows-aarch64-msvc-0.53.1
                                     rust-windows-i686-gnu-0.52.6
                                     rust-windows-i686-gnu-0.53.1
                                     rust-windows-i686-gnullvm-0.52.6
                                     rust-windows-i686-gnullvm-0.53.1
                                     rust-windows-i686-msvc-0.52.6
                                     rust-windows-i686-msvc-0.53.1
                                     rust-windows-x86-64-gnu-0.52.6
                                     rust-windows-x86-64-gnu-0.53.1
                                     rust-windows-x86-64-gnullvm-0.52.6
                                     rust-windows-x86-64-gnullvm-0.53.1
                                     rust-windows-x86-64-msvc-0.52.6
                                     rust-windows-x86-64-msvc-0.53.1
                                     rust-winnow-0.7.14
                                     rust-wit-bindgen-0.46.0
                                     rust-writeable-0.6.2
                                     rust-yansi-1.0.1
                                     rust-yoke-0.8.1
                                     rust-yoke-derive-0.8.1
                                     rust-zerofrom-0.1.6
                                     rust-zerofrom-derive-0.1.6
                                     rust-zeroize-1.8.2
                                     rust-zeroize-derive-1.4.2
                                     rust-zerotrie-0.2.3
                                     rust-zerovec-0.11.5
                                     rust-zerovec-derive-0.11.2
                                     rust-zip-6.0.0
                                     rust-zlib-rs-0.5.4
                                     rust-zopfli-0.8.3
                                     rust-zstd-0.13.3
                                     rust-zstd-safe-7.2.4
                                     rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (supergfxctl =>
                                  (list rust-addr2line-0.24.2
                                   rust-adler2-2.0.0
                                   rust-aho-corasick-1.1.3
                                   rust-anstream-0.6.18
                                   rust-anstyle-1.0.10
                                   rust-anstyle-parse-0.2.6
                                   rust-anstyle-query-1.1.2
                                   rust-anstyle-wincon-3.0.6
                                   rust-async-broadcast-0.7.1
                                   rust-async-channel-2.3.1
                                   rust-async-executor-1.13.1
                                   rust-async-fs-2.1.2
                                   rust-async-io-2.4.0
                                   rust-async-lock-3.4.0
                                   rust-async-process-2.3.0
                                   rust-async-recursion-1.1.1
                                   rust-async-signal-0.2.10
                                   rust-async-task-4.7.1
                                   rust-async-trait-0.1.83
                                   rust-atomic-waker-1.1.2
                                   rust-autocfg-1.4.0
                                   rust-backtrace-0.3.74
                                   rust-bitflags-2.6.0
                                   rust-blocking-1.6.1
                                   rust-bytes-1.8.0
                                   rust-cfg-if-1.0.0
                                   rust-cfg-aliases-0.2.1
                                   rust-colorchoice-1.0.3
                                   rust-concurrent-queue-2.5.0
                                   rust-crossbeam-utils-0.8.20
                                   rust-endi-1.1.0
                                   rust-enumflags2-0.7.10
                                   rust-enumflags2-derive-0.7.10
                                   rust-env-filter-0.1.2
                                   rust-env-logger-0.11.5
                                   rust-equivalent-1.0.1
                                   rust-errno-0.3.9
                                   rust-event-listener-5.3.1
                                   rust-event-listener-strategy-0.5.2
                                   rust-fastrand-2.2.0
                                   rust-futures-core-0.3.31
                                   rust-futures-io-0.3.31
                                   rust-futures-lite-2.6.0
                                   rust-futures-macro-0.3.31
                                   rust-futures-task-0.3.31
                                   rust-futures-util-0.3.31
                                   rust-gimli-0.31.1
                                   rust-gumdrop-0.8.1
                                   rust-gumdrop-derive-0.8.1
                                   rust-hashbrown-0.15.2
                                   rust-hermit-abi-0.3.9
                                   rust-hermit-abi-0.4.0
                                   rust-hex-0.4.3
                                   rust-humantime-2.1.0
                                   rust-indexmap-2.6.0
                                   rust-io-lifetimes-1.0.11
                                   rust-is-terminal-polyfill-1.70.1
                                   rust-itoa-1.0.14
                                   rust-libc-0.2.166
                                   rust-libudev-sys-0.1.4
                                   rust-linux-raw-sys-0.4.14
                                   rust-log-0.4.22
                                   rust-logind-zbus-5.2.0
                                   rust-memchr-2.7.4
                                   rust-memoffset-0.9.1
                                   rust-miniz-oxide-0.8.0
                                   rust-mio-1.0.2
                                   rust-nix-0.29.0
                                   rust-object-0.36.5
                                   rust-once-cell-1.20.2
                                   rust-ordered-stream-0.2.0
                                   rust-parking-2.2.1
                                   rust-pin-project-lite-0.2.15
                                   rust-pin-utils-0.1.0
                                   rust-piper-0.2.4
                                   rust-pkg-config-0.3.31
                                   rust-polling-3.7.4
                                   rust-proc-macro-crate-3.2.0
                                   rust-proc-macro2-1.0.92
                                   rust-quote-1.0.37
                                   rust-regex-1.11.1
                                   rust-regex-automata-0.4.9
                                   rust-regex-syntax-0.8.5
                                   rust-rustc-demangle-0.1.24
                                   rust-rustix-0.38.41
                                   rust-ryu-1.0.18
                                   rust-serde-1.0.215
                                   rust-serde-derive-1.0.215
                                   rust-serde-json-1.0.133
                                   rust-serde-repr-0.1.19
                                   rust-signal-hook-registry-1.4.2
                                   rust-slab-0.4.9
                                   rust-socket2-0.5.7
                                   rust-static-assertions-1.1.0
                                   rust-syn-1.0.109
                                   rust-syn-2.0.89
                                   rust-tempfile-3.14.0
                                   rust-tokio-1.41.1
                                   rust-tokio-macros-2.4.0
                                   rust-toml-datetime-0.6.8
                                   rust-toml-edit-0.22.22
                                   rust-tracing-0.1.41
                                   rust-tracing-attributes-0.1.28
                                   rust-tracing-core-0.1.33
                                   rust-udev-0.9.1
                                   rust-uds-windows-1.1.0
                                   rust-unicode-ident-1.0.14
                                   rust-utf8parse-0.2.2
                                   rust-wasi-0.11.0+wasi-snapshot-preview1
                                   rust-winapi-0.3.9
                                   rust-winapi-i686-pc-windows-gnu-0.4.0
                                   rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                   rust-windows-sys-0.48.0
                                   rust-windows-sys-0.52.0
                                   rust-windows-sys-0.59.0
                                   rust-windows-targets-0.48.5
                                   rust-windows-targets-0.52.6
                                   rust-windows-aarch64-gnullvm-0.48.5
                                   rust-windows-aarch64-gnullvm-0.52.6
                                   rust-windows-aarch64-msvc-0.48.5
                                   rust-windows-aarch64-msvc-0.52.6
                                   rust-windows-i686-gnu-0.48.5
                                   rust-windows-i686-gnu-0.52.6
                                   rust-windows-i686-gnullvm-0.52.6
                                   rust-windows-i686-msvc-0.48.5
                                   rust-windows-i686-msvc-0.52.6
                                   rust-windows-x86-64-gnu-0.48.5
                                   rust-windows-x86-64-gnu-0.52.6
                                   rust-windows-x86-64-gnullvm-0.48.5
                                   rust-windows-x86-64-gnullvm-0.52.6
                                   rust-windows-x86-64-msvc-0.48.5
                                   rust-windows-x86-64-msvc-0.52.6
                                   rust-winnow-0.6.20
                                   rust-winnow-0.7.2
                                   rust-xdg-home-1.3.0
                                   rust-zbus-5.5.0
                                   rust-zbus-macros-5.5.0
                                   rust-zbus-names-4.1.0
                                   rust-zvariant-5.1.0
                                   rust-zvariant-derive-5.1.0
                                   rust-zvariant-utils-3.2.0)))
