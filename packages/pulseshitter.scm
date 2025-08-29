(define-module (packages pulseshitter)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xiph)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix build-system cargo))

(define rust-addr2line-0.21.0
  (crate-source "addr2line" "0.21.0"
                "1jx0k3iwyqr8klqbzk6kjvr496yd94aspis10vwsj5wy7gib4c4a"))

(define rust-adler-1.0.2
  (crate-source "adler" "1.0.2"
                "1zim79cvzd5yrkzl3nyfx0avijwgk9fqv3yrscdy1cc79ih02qpj"))

(define rust-aead-0.4.3
  (crate-source "aead" "0.4.3"
                "0xw8kp9j1whfdxhgmr2qf9xgslkg52zh6gzmhsh13y9w3s73nq8b"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-arrayvec-0.7.4
  (crate-source "arrayvec" "0.7.4"
                "04b7n722jij0v3fnm3qk072d5ysc2q30rl9fz33zpfhzah30mlwn"))

(define rust-async-trait-0.1.80
  (crate-source "async-trait" "0.1.80"
                "1jip2xkv8l67bbg6jrz3b1sdb7api77vy38wrjl7sfkmya3j1yn6"))

(define rust-async-tungstenite-0.17.2
  (crate-source "async-tungstenite" "0.17.2"
                "1fzc5f5zsyxxyk2r88p582lb2fm89wlgxvzkgn7alhqnaqqipdx1"))

(define rust-atomic-waker-1.1.2
  (crate-source "atomic-waker" "1.1.2"
                "1h5av1lw56m0jf0fd3bchxq8a30xv0b4wv8s4zkp4s0i7mfvs18m"))

(define rust-audiopus-0.3.0-rc.0
  (crate-source "audiopus" "0.3.0-rc.0"
                "0m9r8zk3n7r4x1p2fsmy6gn2axrd2bdyai7mb4yxxinpaq7fnmdb"))

(define rust-audiopus-sys-0.2.2
  ;; TODO: Check bundled sources.
  (crate-source "audiopus_sys" "0.2.2"
                "0lc6kzdw65kbgqaghig99f8642k2ikl5imk56q1lw1m28qallcb2"))

(define rust-autocfg-1.3.0
  (crate-source "autocfg" "1.3.0"
                "1c3njkfzpil03k92q0mij5y1pkhhfr4j3bf0h53bgl2vs85lsjqc"))

(define rust-backtrace-0.3.71
  (crate-source "backtrace" "0.3.71"
                "17bgd7pbjb9gc8q47qwsg2lmy9i62x3bsjmmnjrwh5z8s805ic16"))

(define rust-base64-0.13.1
  (crate-source "base64" "0.13.1"
                "1s494mqmzjb766fy1kqlccgfg2sdcjb6hzbvzqv2jw65fdi5h6wy"))

(define rust-base64-0.21.7
  (crate-source "base64" "0.21.7"
                "0rw52yvsk75kar9wgqfwgb414kvil1gn7mqkrhn9zf1537mpsacx"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.5.0
  (crate-source "bitflags" "2.5.0"
                "1h91vdx1il069vdiiissj8ymzj130rbiic0dbs77yxjgjim9sjyg"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-bumpalo-3.16.0
  (crate-source "bumpalo" "3.16.0"
                "0b015qb4knwanbdlp1x48pkb4pm57b8gidbhhhxr900q2wb6fabr"))

(define rust-bytemuck-1.16.0
  (crate-source "bytemuck" "1.16.0"
                "19dwdvjri09mhgrngy0737965pchm25ix2yma8sgwpjxrcalr0vq"))

(define rust-byteorder-1.5.0
  (crate-source "byteorder" "1.5.0"
                "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z"))

(define rust-bytes-1.6.0
  (crate-source "bytes" "1.6.0"
                "1jf2awc1fywpk15m6pxay3wqcg65ararg9xi4b08vnszwiyy2kai"))

(define rust-cassowary-0.3.0
  (crate-source "cassowary" "0.3.0"
                "0lvanj0gsk6pc1chqrh4k5k0vi1rfbgzmsk46dwy3nmrqyw711nz"))

(define rust-cc-1.0.97
  (crate-source "cc" "1.0.97"
                "1d6rv3nk5q6lrr3mf7lifqpjr44slylsz3pw6pmn2k2cv1bm76h9"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-cipher-0.3.0
  (crate-source "cipher" "0.3.0"
                "1dyzsv0c84rgz98d5glnhsz4320wl24x3bq511vnyf0mxir21rby"))

(define rust-cmake-0.1.50
  (crate-source "cmake" "0.1.50"
                "0c3i3548mqbizpgbff94jjgkcd2p6q9fxjjh89zzf5dqcfaph753"))

(define rust-core-foundation-0.9.4
  (crate-source "core-foundation" "0.9.4"
                "13zvbbj07yk3b61b8fhwfzhy35535a583irf23vlcg59j7h9bqci"))

(define rust-core-foundation-sys-0.8.6
  ;; TODO: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.6"
                "13w6sdf06r0hn7bx2b45zxsg1mm2phz34jikm6xc5qrbr6djpsh6"))

(define rust-cpufeatures-0.2.12
  (crate-source "cpufeatures" "0.2.12"
                "012m7rrak4girqlii3jnqwrr73gv1i980q4wra5yyyhvzwk5xzjk"))

(define rust-crc32fast-1.4.0
  (crate-source "crc32fast" "1.4.0"
                "1ahy259ypc955l5ak24hdlgllb6vm6y2pvwr6qrlyisbg255m1dk"))

(define rust-crossbeam-0.8.4
  (crate-source "crossbeam" "0.8.4"
                "1a5c7yacnk723x0hfycdbl91ks2nxhwbwy46b8y5vyy0gxzcsdqi"))

(define rust-crossbeam-channel-0.5.12
  (crate-source "crossbeam-channel" "0.5.12"
                "159gp30clv2ci3r473659ii04pjznspb3g9gwkhj2lavkhmb0gdb"))

(define rust-crossbeam-deque-0.8.5
  (crate-source "crossbeam-deque" "0.8.5"
                "03bp38ljx4wj6vvy4fbhx41q8f585zyqix6pncz1mkz93z08qgv1"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-queue-0.3.11
  (crate-source "crossbeam-queue" "0.3.11"
                "0d8y8y3z48r9javzj67v3p2yfswd278myz1j9vzc4sp7snslc0yz"))

(define rust-crossbeam-utils-0.8.19
  (crate-source "crossbeam-utils" "0.8.19"
                "0iakrb1b8fjqrag7wphl94d10irhbh2fw1g444xslsywqyn3p3i4"))

(define rust-crossterm-0.25.0
  (crate-source "crossterm" "0.25.0"
                "0rsbkkhdf61aipc06b7vpl4cw3wnxz0miizp0ms3a5rcpq7nqkp6"))

(define rust-crossterm-winapi-0.9.1
  (crate-source "crossterm_winapi" "0.9.1"
                "0axbfb2ykbwbpf1hmxwpawwfs8wvmkcka5m561l7yp36ldi7rpdc"))

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

(define rust-dashmap-5.5.3
  (crate-source "dashmap" "5.5.3"
                "0miqnlxi501vfbv6mw5jbmzgnj0wjrch3p4abvpd59s9v30lg1wp"))

(define rust-deranged-0.3.11
  (crate-source "deranged" "0.3.11"
                "1d1ibqqnr5qdrpw8rclwrf1myn3wf0dygl04idf4j2s49ah6yaxl"))

(define rust-derivative-2.2.0
  (crate-source "derivative" "2.2.0"
                "02vpb81wisk2zh1d5f44szzxamzinqgq2k8ydrfjj2wwkrgdvhzw"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-discortp-0.4.0
  (crate-source "discortp" "0.4.0"
                "0wk9s8jm32c0b1sysh8sgxj2z5dmig0kmmihxasj10m48rv02rpv"))

(define rust-encoding-rs-0.8.34
  (crate-source "encoding_rs" "0.8.34"
                "0nagpi1rjqdpvakymwmnlxzq908ncg868lml5b70n08bm82fjpdl"))

(define rust-enum-iterator-1.5.0
  (crate-source "enum-iterator" "1.5.0"
                "153fsc92sz80ghpmbk430vpb9cjaggan592a6njs07dyk7rl5llz"))

(define rust-enum-iterator-derive-1.4.0
  (crate-source "enum-iterator-derive" "1.4.0"
                "0nz6kz8jz2w1vy4y3r0mb8pa5nj3y77mdxdn3b38db322cf9kax1"))

(define rust-enum-primitive-0.1.1
  (crate-source "enum_primitive" "0.1.1"
                "100ip2p3w1rq0clca2ai5shhvpxfipnsjncj0f9ralad5w4m2idy"))

(define rust-equivalent-1.0.1
  (crate-source "equivalent" "1.0.1"
                "1malmx5f4lkfvqasz319lq6gb3ddg19yzf9s8cykfsgzdmyq0hsl"))

(define rust-errno-0.3.9
  (crate-source "errno" "0.3.9"
                "1fi0m0493maq1jygcf1bya9cymz2pc1mqxj26bdv7yjd37v5qk2k"))

(define rust-fastrand-2.1.0
  (crate-source "fastrand" "2.1.0"
                "06p5d0rxq7by260m4ym9ial0bwgi0v42lrvhl6nm2g7h0h2m3h4z"))

(define rust-flate2-1.0.30
  (crate-source "flate2" "1.0.30"
                "1bjx56n0wq5w7vsjn7b5rbmqiw0vc3mfzz1rl7i2jy0wzmy44m2z"))

(define rust-flume-0.10.14
  (crate-source "flume" "0.10.14"
                "0xvm1wpzkjvf99jxy9jp3dxw5nipa9blg7j0ngvxj0rl3i2b8mqn"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-foreign-types-0.3.2
  (crate-source "foreign-types" "0.3.2"
                "1cgk0vyd7r45cj769jym4a6s7vwshvd0z4bqrb92q1fwibmkkwzn"))

(define rust-foreign-types-shared-0.1.1
  (crate-source "foreign-types-shared" "0.1.1"
                "0jxgzd04ra4imjv8jgkmdq59kj8fsz6w4zxsbmlai34h26225c00"))

(define rust-form-urlencoded-1.2.1
  (crate-source "form_urlencoded" "1.2.1"
                "0milh8x7nl4f450s3ddhg57a3flcv6yq8hlkyk6fyr3mcb128dp1"))

(define rust-futures-0.3.30
  (crate-source "futures" "0.3.30"
                "1c04g14bccmprwsvx2j9m2blhwrynq7vhl151lsvcv4gi0b6jp34"))

(define rust-futures-channel-0.3.30
  (crate-source "futures-channel" "0.3.30"
                "0y6b7xxqdjm9hlcjpakcg41qfl7lihf6gavk8fyqijsxhvbzgj7a"))

(define rust-futures-core-0.3.30
  (crate-source "futures-core" "0.3.30"
                "07aslayrn3lbggj54kci0ishmd1pr367fp7iks7adia1p05miinz"))

(define rust-futures-executor-0.3.30
  (crate-source "futures-executor" "0.3.30"
                "07dh08gs9vfll2h36kq32q9xd86xm6lyl9xikmmwlkqnmrrgqxm5"))

(define rust-futures-io-0.3.30
  (crate-source "futures-io" "0.3.30"
                "1hgh25isvsr4ybibywhr4dpys8mjnscw4wfxxwca70cn1gi26im4"))

(define rust-futures-macro-0.3.30
  (crate-source "futures-macro" "0.3.30"
                "1b49qh9d402y8nka4q6wvvj0c88qq91wbr192mdn5h54nzs0qxc7"))

(define rust-futures-sink-0.3.30
  (crate-source "futures-sink" "0.3.30"
                "1dag8xyyaya8n8mh8smx7x6w2dpmafg2din145v973a3hw7f1f4z"))

(define rust-futures-task-0.3.30
  (crate-source "futures-task" "0.3.30"
                "013h1724454hj8qczp8vvs10qfiqrxr937qsrv6rhii68ahlzn1q"))

(define rust-futures-util-0.3.30
  (crate-source "futures-util" "0.3.30"
                "0j0xqhcir1zf2dcbpd421kgw6wvsk0rpxflylcysn1rlp3g02r1x"))

(define rust-generator-0.7.5
  (crate-source "generator" "0.7.5"
                "0knp7cllkf90l4wmfbhmb3c0ha9xn9afrzmphain1d12zy26bhaw"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-getrandom-0.2.15
  (crate-source "getrandom" "0.2.15"
                "1mzlnrb3dgyd1fb84gvw10pyr8wdqdl4ry4sr64i1s8an66pqmn4"))

(define rust-gimli-0.28.1
  (crate-source "gimli" "0.28.1"
                "0lv23wc8rxvmjia3mcxc6hj9vkqnv1bqq0h8nzjcgf71mrxx6wa2"))

(define rust-h2-0.3.26
  (crate-source "h2" "0.3.26"
                "1s7msnfv7xprzs6xzfj5sg6p8bjcdpcqcmjjbkd345cyi1x55zl1"))

(define rust-h2-0.4.5
  (crate-source "h2" "0.4.5"
                "1aw62k5xnghpryjnlsbxxhjiwpxir6yi15l3y02w933s225f50ps"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

(define rust-hermit-abi-0.3.9
  (crate-source "hermit-abi" "0.3.9"
                "092hxjbjnq5fmz66grd9plxd0sh6ssg5fhgwwwqbrzgzkjwdycfj"))

(define rust-http-0.2.12
  (crate-source "http" "0.2.12"
                "1w81s4bcbmcj9bjp7mllm8jlz6b31wzvirz8bgpzbqkpwmbvn730"))

(define rust-http-1.1.0
  (crate-source "http" "1.1.0"
                "0n426lmcxas6h75c2cp25m933pswlrfjz10v91vc62vib2sdvf91"))

(define rust-http-body-0.4.6
  (crate-source "http-body" "0.4.6"
                "1lmyjfk6bqk6k9gkn1dxq770sb78pqbqshga241hr5p995bb5skw"))

(define rust-http-body-1.0.0
  (crate-source "http-body" "1.0.0"
                "0hyn8n3iadrbwq8y0p1rl1275s4nm49bllw5wji29g4aa3dqbb0w"))

(define rust-http-body-util-0.1.1
  (crate-source "http-body-util" "0.1.1"
                "07agldas2qgcfc05ckiarlmf9vzragbda823nqhrqrc6mjrghx84"))

(define rust-httparse-1.8.0
  (crate-source "httparse" "1.8.0"
                "010rrfahm1jss3p022fqf3j3jmm72vhn4iqhykahb9ynpaag75yq"))

(define rust-httpdate-1.0.3
  (crate-source "httpdate" "1.0.3"
                "1aa9rd2sac0zhjqh24c9xvir96g188zldkx0hr6dnnlx5904cfyz"))

(define rust-hyper-0.14.28
  (crate-source "hyper" "0.14.28"
                "107gkvqx4h9bl17d602zkm2dgpfq86l2dr36yzfsi8l3xcsy35mz"))

(define rust-hyper-1.3.1
  (crate-source "hyper" "1.3.1"
                "0va9pjqshsr8zc07m9h4j2821hsmd9lw9j416yisjqh8gp8msmzy"))

(define rust-hyper-rustls-0.24.2
  (crate-source "hyper-rustls" "0.24.2"
                "1475j4a2nczz4aajzzsq3hpwg1zacmzbqg393a14j80ff8izsgpc"))

(define rust-hyper-tls-0.6.0
  (crate-source "hyper-tls" "0.6.0"
                "1q36x2yps6hhvxq5r7mc8ph9zz6xlb573gx0x3yskb0fi736y83h"))

(define rust-hyper-util-0.1.5
  (crate-source "hyper-util" "0.1.5"
                "0mhw6n1s40ifr5a8z0nij2yirpdi81jhcyxfp7byb5hblqj5k1vv"))

(define rust-idna-0.5.0
  (crate-source "idna" "0.5.0"
                "1xhjrcjqq0l5bpzvdgylvpkgk94panxgsirzhjnnqfdgc4a9nkb3"))

(define rust-indexmap-2.2.6
  (crate-source "indexmap" "2.2.6"
                "09hgwi2ig0wyj5rjziia76zmhgfj95k0jb4ic3iiawm4vlavg3qn"))

(define rust-ipnet-2.9.0
  (crate-source "ipnet" "2.9.0"
                "1hzrcysgwf0knf83ahb3535hrkw63mil88iqc6kjaryfblrqylcg"))

(define rust-itoa-1.0.11
  (crate-source "itoa" "1.0.11"
                "0nv9cqjwzr3q58qz84dcz63ggc54yhf1yqar1m858m1kfd4g3wa9"))

(define rust-js-sys-0.3.69
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.69"
                "0v99rz97asnzapb0jsc3jjhvxpfxr7h7qd97yqyrf9i7viimbh99"))

(define rust-lazy-static-1.4.0
  (crate-source "lazy_static" "1.4.0"
                "0in6ikhw8mgl33wjv6q6xfrb5b9jr16q8ygjy803fay4zcisvaz2"))

(define rust-libc-0.2.154
  (crate-source "libc" "0.2.154"
                "0inkwrnwzrr1kw75x944ihdajrhhldkgg4irx1n19y9gp4w36x5f"))

(define rust-libpulse-binding-2.28.1
  (crate-source "libpulse-binding" "2.28.1"
                "1zza12f22wf1qs6h71lq1i73aj3kmv3036hqc7qci063vyi5fdgd"))

(define rust-libpulse-sys-1.21.0
  ;; TODO: Check bundled sources.
  (crate-source "libpulse-sys" "1.21.0"
                "16vs0qk6xadckb5qxlrhg0f4jn2zakfd7xih1lk1fb7lzc8f26dw"))

(define rust-linux-raw-sys-0.4.14
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.14"
                "12gsjgbhhjwywpqcrizv80vrp7p7grsz5laqq773i33wphjsxcvq"))

(define rust-lock-api-0.4.12
  (crate-source "lock_api" "0.4.12"
                "05qvxa6g27yyva25a5ghsg85apdxkvr77yhkyhapj6r8vnf8pbq7"))

(define rust-log-0.4.21
  (crate-source "log" "0.4.21"
                "074hldq1q8rlzq2s2qa8f25hj4s3gpw71w64vdwzjd01a4g8rvch"))

(define rust-loom-0.5.6
  (crate-source "loom" "0.5.6"
                "1x9hmiv00ir79kypcg0jdw0j0fkd3ymq6rpv6pli6q5qifrfql7z"))

(define rust-matchers-0.1.0
  (crate-source "matchers" "0.1.0"
                "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2"))

(define rust-memchr-2.7.2
  (crate-source "memchr" "2.7.2"
                "07bcqxb0vx4ji0648ny5xsicjnpma95x1n07v7mi7jrhsz2l11kc"))

(define rust-mime-0.3.17
  (crate-source "mime" "0.3.17"
                "16hkibgvb9klh0w0jk5crr5xv90l3wlf77ggymzjmvl1818vnxv8"))

(define rust-mime-guess-2.0.4
  (crate-source "mime_guess" "2.0.4"
                "1vs28rxnbfwil6f48hh58lfcx90klcvg68gxdc60spwa4cy2d4j1"))

(define rust-miniz-oxide-0.7.2
  (crate-source "miniz_oxide" "0.7.2"
                "19qlxb21s6kabgqq61mk7kd1qk2invyygj076jz6i1gj2lz1z0cx"))

(define rust-mio-0.8.11
  (crate-source "mio" "0.8.11"
                "034byyl0ardml5yliy1hmvx8arkmn9rv479pid794sm07ia519m4"))

(define rust-multiversion-0.7.4
  (crate-source "multiversion" "0.7.4"
                "0hm1y7dhdbam2yvaxmxzd0bj7gv777y0zn82jjzx0fhxl5hi31f4"))

(define rust-multiversion-macros-0.7.4
  (crate-source "multiversion-macros" "0.7.4"
                "142yhgdxvy9qjyi8n4wg2hi1dsckay816g1jg0jpvhp0x7g4v9vr"))

(define rust-nanorand-0.7.0
  (crate-source "nanorand" "0.7.0"
                "1hr60b8zlfy7mxjcwx2wfmhpkx7vfr3v9x12shmv1c10b0y32lba"))

(define rust-native-tls-0.2.12
  (crate-source "native-tls" "0.2.12"
                "0rkl65z70n7sy4d5w0qa99klg1hr43wx6kcprk4d2n9xr2r4wqd8"))

(define rust-nu-ansi-term-0.46.0
  (crate-source "nu-ansi-term" "0.46.0"
                "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))

(define rust-num-conv-0.1.0
  (crate-source "num-conv" "0.1.0"
                "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))

(define rust-num-cpus-1.16.0
  (crate-source "num_cpus" "1.16.0"
                "0hra6ihpnh06dvfvz9ipscys0xfqa9ca9hzp384d5m02ssvgqqa1"))

(define rust-num-derive-0.3.3
  (crate-source "num-derive" "0.3.3"
                "0gbl94ckzqjdzy4j8b1p55mz01g6n1l9bckllqvaj0wfz7zm6sl7"))

(define rust-num-traits-0.1.43
  (crate-source "num-traits" "0.1.43"
                "0c9whknf2dm74a3cqirafy6gj83a76gl56g4v3g19k6lkwz13rcj"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-object-0.32.2
  (crate-source "object" "0.32.2"
                "0hc4cjwyngiy6k51hlzrlsxgv5z25vv7c2cp0ky1lckfic0259m6"))

(define rust-once-cell-1.19.0
  (crate-source "once_cell" "1.19.0"
                "14kvw7px5z96dk4dwdm1r9cqhhy2cyj1l5n5b29mynbb8yr15nrz"))

(define rust-opaque-debug-0.3.1
  (crate-source "opaque-debug" "0.3.1"
                "10b3w0kydz5jf1ydyli5nv10gdfp97xh79bgz327d273bs46b3f0"))

(define rust-openssl-0.10.64
  (crate-source "openssl" "0.10.64"
                "07vb455yh08qh3n493ssw1qsa3zg3zfj438kk2180453hq94i84m"))

(define rust-openssl-macros-0.1.1
  (crate-source "openssl-macros" "0.1.1"
                "173xxvfc63rr5ybwqwylsir0vq6xsj4kxiv4hmg4c3vscdmncj59"))

(define rust-openssl-probe-0.1.5
  (crate-source "openssl-probe" "0.1.5"
                "1kq18qm48rvkwgcggfkqq6pm948190czqc94d6bm2sir5hq1l0gz"))

(define rust-openssl-sys-0.9.102
  ;; TODO: Check bundled sources.
  (crate-source "openssl-sys" "0.9.102"
                "18jzni7xzdcqwf9r8kp6j46abrxqn82dvc2ylf9kij7varyn75y5"))

(define rust-ordered-float-2.10.1
  (crate-source "ordered-float" "2.10.1"
                "075i108hr95pr7hy4fgxivib5pky3b6b22rywya5qyd2wmkrvwb8"))

(define rust-overload-0.1.1
  (crate-source "overload" "0.1.1"
                "0fdgbaqwknillagy1xq7xfgv60qdbk010diwl7s1p0qx7hb16n5i"))

(define rust-parking-lot-0.12.2
  (crate-source "parking_lot" "0.12.2"
                "1ys2dzz6cysjmwyivwxczl1ljpcf5cj4qmhdj07d5bkc9z5g0jky"))

(define rust-parking-lot-core-0.9.10
  (crate-source "parking_lot_core" "0.9.10"
                "1y3cf9ld9ijf7i4igwzffcn0xl16dxyn4c5bwgjck1dkgabiyh0y"))

(define rust-percent-encoding-2.3.1
  (crate-source "percent-encoding" "2.3.1"
                "0gi8wgx0dcy8rnv1kywdv98lwcx67hz0a0zwpib5v2i08r88y573"))

(define rust-pin-project-1.1.5
  (crate-source "pin-project" "1.1.5"
                "1cxl146x0q7lawp0m1826wsgj8mmmfs6ja8q7m6f7ff5j6vl7gxn"))

(define rust-pin-project-internal-1.1.5
  (crate-source "pin-project-internal" "1.1.5"
                "0r9r4ivwiyqf45sv6b30l1dx282lxaax2f6gl84jwa3q590s8f1g"))

(define rust-pin-project-lite-0.2.14
  (crate-source "pin-project-lite" "0.2.14"
                "00nx3f04agwjlsmd3mc5rx5haibj2v8q9b52b0kwn63wcv4nz9mx"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

(define rust-pkg-config-0.3.30
  (crate-source "pkg-config" "0.3.30"
                "1v07557dj1sa0aly9c90wsygc0i8xv5vnmyv0g94lpkvj8qb4cfj"))

(define rust-pnet-base-0.28.0
  (crate-source "pnet_base" "0.28.0"
                "1f3g2p8zgmgsgyzjai4mv0nl22k1lxlrzz7zlsmdqlx7a7aqqj15"))

(define rust-pnet-macros-0.28.0
  (crate-source "pnet_macros" "0.28.0"
                "0j3mslgg6c1fdgsnqk9s0a9z894an2brilz0zaw05175a840wj9h"))

(define rust-pnet-macros-support-0.28.0
  (crate-source "pnet_macros_support" "0.28.0"
                "144gxxamd1904azjlrlky324mp9h5n7h9kmd0lq05aqcyc84wwfl"))

(define rust-poly1305-0.7.2
  (crate-source "poly1305" "0.7.2"
                "1pkf4jlriskq9rvz8y5fjj9dw42q6yg5djijlin4n6p1dd3yp2h4"))

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

(define rust-ppv-lite86-0.2.17
  (crate-source "ppv-lite86" "0.2.17"
                "1pp6g52aw970adv3x2310n7glqnji96z0a9wiamzw89ibf0ayh2v"))

(define rust-proc-macro2-1.0.82
  (crate-source "proc-macro2" "1.0.82"
                "06qk88hbf6wg4v1i961zibhjz512873jwkz3myx1z82ip6dd9lwa"))

(define rust-quote-1.0.36
  (crate-source "quote" "1.0.36"
                "19xcmh445bg6simirnnd4fvkmp6v2qiwxh5f6rw4a70h76pnm9qg"))

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-redox-syscall-0.5.1
  (crate-source "redox_syscall" "0.5.1"
                "0zja6y3av9z50gg1hh0vsc053941wng21r43whhk8mfb9n4m5426"))

(define rust-regex-1.10.4
  (crate-source "regex" "1.10.4"
                "0k5sb0h2mkwf51ab0gvv3x38jp1q7wgxf63abfbhi0wwvvgxn5y1"))

(define rust-regex-automata-0.1.10
  (crate-source "regex-automata" "0.1.10"
                "0ci1hvbzhrfby5fdpf4ganhf7kla58acad9i1ff1p34dzdrhs8vc"))

(define rust-regex-automata-0.4.6
  (crate-source "regex-automata" "0.4.6"
                "1spaq7y4im7s56d1gxa2hi4hzf6dwswb1bv8xyavzya7k25kpf46"))

(define rust-regex-syntax-0.6.29
  (crate-source "regex-syntax" "0.6.29"
                "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi"))

(define rust-regex-syntax-0.8.3
  (crate-source "regex-syntax" "0.8.3"
                "0mhzkm1pkqg6y53xv056qciazlg47pq0czqs94cn302ckvi49bdd"))

(define rust-reqwest-0.11.27
  (crate-source "reqwest" "0.11.27"
                "0qjary4hpplpgdi62d2m0xvbn6lnzckwffm0rgkm2x51023m6ryx"))

(define rust-reqwest-0.12.4
  (crate-source "reqwest" "0.12.4"
                "047aa0qnngnlnf9i0abrs6pgmz15vk81p5pvscwhk3l6jbfsyv2n"))

(define rust-ring-0.16.20
  (crate-source "ring" "0.16.20"
                "1z682xp7v38ayq9g9nkbhhfpj6ygralmlx7wdmsfv8rnw99cylrh"))

(define rust-ring-0.17.8
  (crate-source "ring" "0.17.8"
                "03fwlb1ssrmfxdckvqv033pfmk01rhx9ynwi7r186dcfcp5s8zy1"))

(define rust-ringbuf-0.3.3
  (crate-source "ringbuf" "0.3.3"
                "02lmklwymawzfrgp9wy60yk2v3lkyv2p5v0w40la3lhzim1fvavr"))

(define rust-ron-0.8.1
  (crate-source "ron" "0.8.1"
                "154w53s895yxdfg7rn87c6f6x4yncc535x1x31zpcj7p0pzpw7xr"))

(define rust-rustc-demangle-0.1.24
  (crate-source "rustc-demangle" "0.1.24"
                "07zysaafgrkzy2rjgwqdj2a8qdpsm6zv6f5pgpk9x0lm40z9b6vi"))

(define rust-rustix-0.38.34
  (crate-source "rustix" "0.38.34"
                "03vkqa2ism7q56rkifyy8mns0wwqrk70f4i4fd53r97p8b05xp3h"))

(define rust-rustls-0.20.9
  (crate-source "rustls" "0.20.9"
                "16byazb8jfr06kgbijy92bdk0ila806g6a00a6l9x64mqpgf700v"))

(define rust-rustls-0.21.12
  (crate-source "rustls" "0.21.12"
                "0gjdg2a9r81sdwkyw3n5yfbkrr6p9gyk3xr2kcsr3cs83x6s2miz"))

(define rust-rustls-pemfile-1.0.4
  (crate-source "rustls-pemfile" "1.0.4"
                "1324n5bcns0rnw6vywr5agff3rwfvzphi7rmbyzwnv6glkhclx0w"))

(define rust-rustls-pemfile-2.1.2
  (crate-source "rustls-pemfile" "2.1.2"
                "0ggpmk5n7p096nim2hn57facx6rwf76l55qqsj4fny37d0jkm699"))

(define rust-rustls-pki-types-1.7.0
  (crate-source "rustls-pki-types" "1.7.0"
                "0banlc9xzwqrx8n0h4bd0igmq3z5hc72rn941lf22cp3gkkraqlp"))

(define rust-rustls-webpki-0.101.7
  (crate-source "rustls-webpki" "0.101.7"
                "0rapfhpkqp75552i8r0y7f4vq7csb4k7gjjans0df73sxv8paqlb"))

(define rust-rustversion-1.0.17
  (crate-source "rustversion" "1.0.17"
                "1mm3fckyvb0l2209in1n2k05sws5d9mpkszbnwhq3pkq8apjhpcm"))

(define rust-ryu-1.0.18
  (crate-source "ryu" "1.0.18"
                "17xx2s8j1lln7iackzd9p0sv546vjq71i779gphjq923vjh5pjzk"))

(define rust-salsa20-0.9.0
  (crate-source "salsa20" "0.9.0"
                "11i646kpgimimqiq8hyi0b7ngp588f7nl9xsc317d9kdcxgvn3qc"))

(define rust-schannel-0.1.23
  (crate-source "schannel" "0.1.23"
                "0d1m156bsjrws6xzzr1wyfyih9i22mb2csb5pc5kmkrvci2ibjgv"))

(define rust-scoped-tls-1.0.1
  (crate-source "scoped-tls" "1.0.1"
                "15524h04mafihcvfpgxd8f4bgc3k95aclz8grjkg9a0rxcvn9kz1"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-sct-0.7.1
  (crate-source "sct" "0.7.1"
                "056lmi2xkzdg1dbai6ha3n57s18cbip4pnmpdhyljli3m99n216s"))

(define rust-security-framework-2.11.0
  (crate-source "security-framework" "2.11.0"
                "1h0ipvv8wi9wvhad0a9w7jpmb189jng4jhfgnp6vl1lps0zp49y6"))

(define rust-security-framework-sys-2.11.0
  ;; TODO: Check bundled sources.
  (crate-source "security-framework-sys" "2.11.0"
                "1mq7ykz3fi0ba55aj4afz24v9qvwdpkbjiirb197f8h5pnxkcy9i"))

(define rust-serde-1.0.201
  (crate-source "serde" "1.0.201"
                "0g1nrz2s6l36na6gdbph8k07xf9h5p3s6f0s79sy8a8nxpmiq3vq"))

(define rust-serde-derive-1.0.201
  (crate-source "serde_derive" "1.0.201"
                "0r98v8h47s7zhml7gz0sl6wv82vyzh1hv27f1g0g35lp1f9hbr65"))

(define rust-serde-json-1.0.117
  (crate-source "serde_json" "1.0.117"
                "1hxziifjlc0kn1cci9d4crmjc7qwnfi20lxwyj9lzca2c7m84la5"))

(define rust-serde-repr-0.1.19
  (crate-source "serde_repr" "0.1.19"
                "1sb4cplc33z86pzlx38234xr141wr3cmviqgssiadisgl8dlar3c"))

(define rust-serde-urlencoded-0.7.1
  (crate-source "serde_urlencoded" "0.7.1"
                "1zgklbdaysj3230xivihs30qi5vkhigg323a9m62k8jwf4a1qjfk"))

(define rust-serde-value-0.7.0
  (crate-source "serde-value" "0.7.0"
                "0b18ngk7n4f9zmwsfdkhgsp31192smzyl5z143qmx1qi28sa78gk"))

(define rust-serenity-0.11.7
  (crate-source "serenity" "0.11.7"
                "1ii2js99shrpsammck58mn5f2a1rdlgg8bdg9jfzr0rlyb78jyks"))

(define rust-serenity-voice-model-0.1.1
  (crate-source "serenity-voice-model" "0.1.1"
                "0lj93n0wx546glkvwgr8yjsyjs5xdzcvxpx5x3hzv8lwhk4axqwb"))

(define rust-sha-1-0.10.1
  (crate-source "sha-1" "0.10.1"
                "1700fs5aiiailpd5h0ax4sgs2ngys0mqf3p4j0ry6j2p2zd8l1gm"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-signal-hook-0.3.17
  (crate-source "signal-hook" "0.3.17"
                "0098nsah04spqf3n8niirmfym4wsdgjl57c78kmzijlq8xymh8c6"))

(define rust-signal-hook-mio-0.2.3
  (crate-source "signal-hook-mio" "0.2.3"
                "1bwrrbd0lhwzlf63708vyzlh20693s5bg5s0ak6adjbyycajxb99"))

(define rust-signal-hook-registry-1.4.2
  (crate-source "signal-hook-registry" "1.4.2"
                "1cb5akgq8ajnd5spyn587srvs4n26ryq0p78nswffwhv46sf1sd9"))

(define rust-slab-0.4.9
  (crate-source "slab" "0.4.9"
                "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))

(define rust-smallvec-1.13.2
  (crate-source "smallvec" "1.13.2"
                "0rsw5samawl3wsw6glrsb127rx6sh89a8wyikicw6dkdcjd1lpiw"))

(define rust-socket2-0.5.7
  (crate-source "socket2" "0.5.7"
                "070r941wbq76xpy039an4pyiy3rfj7mp7pvibf1rcri9njq5wc6f"))

(define rust-songbird-0.3.2
  (crate-source "songbird" "0.3.2"
                "127jvs5z2b2ph13bjw2pb7hsz7qn8pp3rr533pg3j6bpznh8dxij"))

(define rust-spin-0.5.2
  (crate-source "spin" "0.5.2"
                "0b84m6dbzrwf2kxylnw82d3dr8w06av7rfkr8s85fb5f43rwyqvf"))

(define rust-spin-0.9.8
  (crate-source "spin" "0.9.8"
                "0rvam5r0p3a6qhc18scqpvpgb3ckzyqxpgdfyjnghh8ja7byi039"))

(define rust-streamcatcher-1.0.1
  (crate-source "streamcatcher" "1.0.1"
                "0agazjs1sja5n41jqc7rfcmcm4935pxihqnsixsv1as9qdalfrki"))

(define rust-strsim-0.10.0
  (crate-source "strsim" "0.10.0"
                "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))

(define rust-subtle-2.4.1
  (crate-source "subtle" "2.4.1"
                "00b6jzh9gzb0h9n25g06nqr90z3xzqppfhhb260s1hjhh4pg7pkb"))

(define rust-symphonia-core-0.5.4
  (crate-source "symphonia-core" "0.5.4"
                "1hr2w2a217vq4lpghszmsdwxr5ilh5d1ysfm3cixbirxkrvhd0vr"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.63
  (crate-source "syn" "2.0.63"
                "015pzk421l24x5xzk8dq6wikwvdyldhz5rlbgpxs389wc8qyfnxz"))

(define rust-sync-wrapper-0.1.2
  (crate-source "sync_wrapper" "0.1.2"
                "0q01lyj0gr9a93n10nxsn8lwbzq97jqd6b768x17c8f7v7gccir0"))

(define rust-system-configuration-0.5.1
  (crate-source "system-configuration" "0.5.1"
                "1rz0r30xn7fiyqay2dvzfy56cvaa3km74hnbz2d72p97bkf3lfms"))

(define rust-system-configuration-sys-0.5.0
  ;; TODO: Check bundled sources.
  (crate-source "system-configuration-sys" "0.5.0"
                "1jckxvdr37bay3i9v52izgy52dg690x5xfg3hd394sv2xf4b2px7"))

(define rust-target-features-0.1.6
  (crate-source "target-features" "0.1.6"
                "1m8y0ksw30gnkidjsjvnmhlpj165mgyj8ylk0lbs0qy4qprvkfy1"))

(define rust-tempfile-3.10.1
  (crate-source "tempfile" "3.10.1"
                "1wdzz35ri168jn9al4s1g2rnsrr5ci91khgarc2rvpb3nappzdw5"))

(define rust-thiserror-1.0.60
  (crate-source "thiserror" "1.0.60"
                "067wi7pb1zn9jhhk82w0ppmvjwa00nwkp4m9j77rvpaqra1r17jp"))

(define rust-thiserror-impl-1.0.60
  (crate-source "thiserror-impl" "1.0.60"
                "0945q2hk1rqdzjz2zqakxbddwm4h26k5c0wdncdarhvfq10h0iz2"))

(define rust-thread-local-1.1.8
  (crate-source "thread_local" "1.1.8"
                "173i5lyjh011gsimk21np9jn8al18rxsrkjli20a7b8ks2xgk7lb"))

(define rust-time-0.3.36
  (crate-source "time" "0.3.36"
                "11g8hdpahgrf1wwl2rpsg5nxq3aj7ri6xr672v4qcij6cgjqizax"))

(define rust-time-core-0.1.2
  (crate-source "time-core" "0.1.2"
                "1wx3qizcihw6z151hywfzzyd1y5dl804ydyxci6qm07vbakpr4pg"))

(define rust-time-macros-0.2.18
  (crate-source "time-macros" "0.2.18"
                "1kqwxvfh2jkpg38fy673d6danh1bhcmmbsmffww3mphgail2l99z"))

(define rust-tinyvec-1.6.0
  (crate-source "tinyvec" "1.6.0"
                "0l6bl2h62a5m44jdnpn7lmj14rd44via8180i7121fvm73mmrk47"))

(define rust-tinyvec-macros-0.1.1
  (crate-source "tinyvec_macros" "0.1.1"
                "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z"))

(define rust-tokio-1.37.0
  (crate-source "tokio" "1.37.0"
                "11v7qhvpwsf976frqgrjl1jy308bdkxq195gb38cypx7xkzypnqs"))

(define rust-tokio-macros-2.2.0
  (crate-source "tokio-macros" "2.2.0"
                "0fwjy4vdx1h9pi4g2nml72wi0fr27b5m954p13ji9anyy8l1x2jv"))

(define rust-tokio-native-tls-0.3.1
  (crate-source "tokio-native-tls" "0.3.1"
                "1wkfg6zn85zckmv4im7mv20ca6b1vmlib5xwz9p7g19wjfmpdbmv"))

(define rust-tokio-rustls-0.23.4
  (crate-source "tokio-rustls" "0.23.4"
                "0nfsmmi8l1lgpbfy6079d5i13984djzcxrdr9jc06ghi0cwyhgn4"))

(define rust-tokio-rustls-0.24.1
  (crate-source "tokio-rustls" "0.24.1"
                "10bhibg57mqir7xjhb2xmf24xgfpx6fzpyw720a4ih8a737jg0y2"))

(define rust-tokio-util-0.7.11
  (crate-source "tokio-util" "0.7.11"
                "1qcz30db6m8lxkl61b3nic4bim1symi636nhbb3rmi3i6xxv9xlw"))

(define rust-tower-0.4.13
  (crate-source "tower" "0.4.13"
                "073wncyqav4sak1p755hf6vl66njgfc1z1g1di9rxx3cvvh9pymq"))

(define rust-tower-layer-0.3.2
  (crate-source "tower-layer" "0.3.2"
                "1l7i17k9vlssrdg4s3b0ia5jjkmmxsvv8s9y9ih0jfi8ssz8s362"))

(define rust-tower-service-0.3.2
  (crate-source "tower-service" "0.3.2"
                "0lmfzmmvid2yp2l36mbavhmqgsvzqf7r2wiwz73ml4xmwaf1rg5n"))

(define rust-tracing-0.1.40
  (crate-source "tracing" "0.1.40"
                "1vv48dac9zgj9650pg2b4d0j3w6f3x9gbggf43scq5hrlysklln3"))

(define rust-tracing-attributes-0.1.27
  (crate-source "tracing-attributes" "0.1.27"
                "1rvb5dn9z6d0xdj14r403z0af0bbaqhg02hq4jc97g5wds6lqw1l"))

(define rust-tracing-core-0.1.32
  (crate-source "tracing-core" "0.1.32"
                "0m5aglin3cdwxpvbg6kz0r9r0k31j48n0kcfwsp6l49z26k3svf0"))

(define rust-tracing-futures-0.2.5
  (crate-source "tracing-futures" "0.2.5"
                "1wimg0iwa2ldq7xv98lvivvf3q9ykfminig8r1bs0ig22np9bl4p"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-subscriber-0.3.18
  (crate-source "tracing-subscriber" "0.3.18"
                "12vs1bwk4kig1l2qqjbbn2nm5amwiqmkcmnznylzmnfvjy6083xd"))

(define rust-try-lock-0.2.5
  (crate-source "try-lock" "0.2.5"
                "0jqijrrvm1pyq34zn1jmy2vihd4jcrjlvsh4alkjahhssjnsn8g4"))

(define rust-tui-0.19.0
  (crate-source "tui" "0.19.0"
                "1ldswnqgmdkd2fkislyh1amd6rmnbx3s8b97k9j7w03lsv5jdpfc"))

(define rust-tui-textarea-0.2.4
  (crate-source "tui-textarea" "0.2.4"
                "0pzg9k9mhv5crspqixaqncrbkpik30vpw6hdin7l4zcy1j97rbkq"))

(define rust-tungstenite-0.17.3
  (crate-source "tungstenite" "0.17.3"
                "1q2czb80xb7hp7ipqi5d21716i52k8s7iz18xxzfwaccdbyr4yg2"))

(define rust-typemap-rev-0.1.5
  (crate-source "typemap_rev" "0.1.5"
                "0mg1bljnddkwh8ahy1xq1bgrmc4k8fcvdavr19c58m2blbq78nzd"))

(define rust-typenum-1.17.0
  (crate-source "typenum" "1.17.0"
                "09dqxv69m9lj9zvv6xw5vxaqx15ps0vxyy5myg33i0kbqvq0pzs2"))

(define rust-unicase-2.7.0
  (crate-source "unicase" "2.7.0"
                "12gd74j79f94k4clxpf06l99wiv4p30wjr0qm04ihqk9zgdd9lpp"))

(define rust-unicode-bidi-0.3.15
  (crate-source "unicode-bidi" "0.3.15"
                "0xcdxm7h0ydyprwpcbh436rbs6s6lph7f3gr527lzgv6lw053y88"))

(define rust-unicode-ident-1.0.12
  (crate-source "unicode-ident" "1.0.12"
                "0jzf1znfpb2gx8nr8mvmyqs1crnv79l57nxnbiszc7xf7ynbjm1k"))

(define rust-unicode-normalization-0.1.23
  (crate-source "unicode-normalization" "0.1.23"
                "1x81a50h2zxigj74b9bqjsirxxbyhmis54kg600xj213vf31cvd5"))

(define rust-unicode-segmentation-1.11.0
  (crate-source "unicode-segmentation" "1.11.0"
                "00kjpwp1g8fqm45drmwivlacn3y9jx73bvs09n6s3x73nqi7vj6l"))

(define rust-unicode-width-0.1.12
  (crate-source "unicode-width" "0.1.12"
                "1mk6mybsmi5py8hf8zy9vbgs4rw4gkdqdq3gzywd9kwf2prybxb8"))

(define rust-universal-hash-0.4.1
  (crate-source "universal-hash" "0.4.1"
                "01av09i0rqcl8f0xgvn2g07kzyafgbiwdhkfwq0m14kyd67lw8cz"))

(define rust-untrusted-0.7.1
  (crate-source "untrusted" "0.7.1"
                "0jkbqaj9d3v5a91pp3wp9mffvng1nhycx6sh4qkdd9qyr62ccmm1"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"))

(define rust-url-2.5.0
  (crate-source "url" "2.5.0"
                "0cs65961miawncdg2z20171w0vqrmraswv2ihdpd8lxp7cp31rii"))

(define rust-utf-8-0.7.6
  (crate-source "utf-8" "0.7.6"
                "1a9ns3fvgird0snjkd3wbdhwd3zdpc2h5gpyybrfr6ra5pkqxk09"))

(define rust-uuid-0.8.2
  (crate-source "uuid" "0.8.2"
                "1dy4ldcp7rnzjy56dxh7d2sgrcvn4q77y0a8r0a48946h66zjp5w"))

(define rust-valuable-0.1.0
  (crate-source "valuable" "0.1.0"
                "0v9gp3nkjbl30z0fd56d8mx7w1csk86wwjhfjhr400wh9mfpw2w3"))

(define rust-vcpkg-0.2.15
  (crate-source "vcpkg" "0.2.15"
                "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc"))

(define rust-version-check-0.9.4
  (crate-source "version_check" "0.9.4"
                "0gs8grwdlgh0xq660d7wr80x14vxbizmd8dbp29p2pdncx8lp1s9"))

(define rust-want-0.3.1
  (crate-source "want" "0.3.1"
                "03hbfrnvqqdchb5kgxyavb9jabwza0dmh2vw5kg0dq8rxl57d9xz"))

(define rust-wasi-0.11.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.0+wasi-snapshot-preview1"
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))

(define rust-wasm-bindgen-0.2.92
  (crate-source "wasm-bindgen" "0.2.92"
                "1a4mcw13nsk3fr8fxjzf9kk1wj88xkfsmnm0pjraw01ryqfm7qjb"))

(define rust-wasm-bindgen-backend-0.2.92
  (crate-source "wasm-bindgen-backend" "0.2.92"
                "1nj7wxbi49f0rw9d44rjzms26xlw6r76b2mrggx8jfbdjrxphkb1"))

(define rust-wasm-bindgen-futures-0.4.42
  (crate-source "wasm-bindgen-futures" "0.4.42"
                "1h322zjvpjllcpj7dahfxjsv6inkr6y0baw7nkdwivr1c4v19g3n"))

(define rust-wasm-bindgen-macro-0.2.92
  (crate-source "wasm-bindgen-macro" "0.2.92"
                "09npa1srjjabd6nfph5yc03jb26sycjlxhy0c2a1pdrpx4yq5y51"))

(define rust-wasm-bindgen-macro-support-0.2.92
  (crate-source "wasm-bindgen-macro-support" "0.2.92"
                "1dqv2xs8zcyw4kjgzj84bknp2h76phmsb3n7j6hn396h4ssifkz9"))

(define rust-wasm-bindgen-shared-0.2.92
  (crate-source "wasm-bindgen-shared" "0.2.92"
                "15kyavsrna2cvy30kg03va257fraf9x00ny554vxngvpyaa0q6dg"))

(define rust-wasm-streams-0.4.0
  (crate-source "wasm-streams" "0.4.0"
                "0ad17c59xb8fffsnbrqbyqz93hb66nzxhizpii31icb31g4w8pdn"))

(define rust-web-sys-0.3.69
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.69"
                "1vqkxk935xa8zcnsi4bd88sb267ly2i24xl1yiq26d1n32hskbvp"))

(define rust-webpki-0.22.4
  (crate-source "webpki" "0.22.4"
                "0lwv7jdlcqjjqqhxcrapnyk5bz4lvr12q444b50gzl3krsjswqzd"))

(define rust-webpki-roots-0.22.6
  (crate-source "webpki-roots" "0.22.6"
                "11rd1aj73qzcvdj3x78crm1758sc4wrbc7rh0r8lmhyjsx01xixn"))

(define rust-webpki-roots-0.25.4
  (crate-source "webpki-roots" "0.25.4"
                "1qgqa615gc1cgklls4bkjp9jv9pvv3jnl82lc6wd7dkximywa82z"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-windows-0.48.0
  (crate-source "windows" "0.48.0"
                "03vh89ilnxdxdh0n9np4ns4m10fvm93h3b0cc05ipg3qq1mqi1p6"))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"))

(define rust-windows-aarch64-gnullvm-0.52.5
  (crate-source "windows_aarch64_gnullvm" "0.52.5"
                "0qrjimbj67nnyn7zqy15mzzmqg0mn5gsr2yciqjxm3cb3vbyx23h"))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"))

(define rust-windows-aarch64-msvc-0.52.5
  (crate-source "windows_aarch64_msvc" "0.52.5"
                "1dmga8kqlmln2ibckk6mxc9n59vdg8ziqa2zr8awcl720hazv1cr"))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"))

(define rust-windows-i686-gnu-0.52.5
  (crate-source "windows_i686_gnu" "0.52.5"
                "0w4np3l6qwlra9s2xpflqrs60qk1pz6ahhn91rr74lvdy4y0gfl8"))

(define rust-windows-i686-gnullvm-0.52.5
  (crate-source "windows_i686_gnullvm" "0.52.5"
                "1s9f4gff0cixd86mw3n63rpmsm4pmr4ffndl6s7qa2h35492dx47"))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"))

(define rust-windows-i686-msvc-0.52.5
  (crate-source "windows_i686_msvc" "0.52.5"
                "1gw7fklxywgpnwbwg43alb4hm0qjmx72hqrlwy5nanrxs7rjng6v"))

(define rust-windows-sys-0.48.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.52.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.5
  (crate-source "windows-targets" "0.52.5"
                "1sz7jrnkygmmlj1ia8fk85wbyil450kq5qkh5qh9sh2rcnj161vg"))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"))

(define rust-windows-x86-64-gnu-0.52.5
  (crate-source "windows_x86_64_gnu" "0.52.5"
                "1n8p2mcf3lw6300k77a0knksssmgwb9hynl793mhkzyydgvlchjf"))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"))

(define rust-windows-x86-64-gnullvm-0.52.5
  (crate-source "windows_x86_64_gnullvm" "0.52.5"
                "15n56jrh4s5bz66zimavr1rmcaw6wa306myrvmbc6rydhbj9h8l5"))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"))

(define rust-windows-x86-64-msvc-0.52.5
  (crate-source "windows_x86_64_msvc" "0.52.5"
                "1w1bn24ap8dp9i85s8mlg8cim2bl2368bd6qyvm0xzqvzmdpxi5y"))

(define rust-winreg-0.50.0
  (crate-source "winreg" "0.50.0"
                "1cddmp929k882mdh6i9f2as848f13qqna6czwsqzkh1pqnr5fkjj"))

(define rust-winreg-0.52.0
  (crate-source "winreg" "0.52.0"
                "19gh9vp7mp1ab84kc3ag48nm9y7xgjhh3xa4vxss1gylk1rsaxx2"))

(define rust-xsalsa20poly1305-0.8.0
  (crate-source "xsalsa20poly1305" "0.8.0"
                "11h87km1621v8gwy458wl2cx5p075yhwx58b8n8h0rbcbnbcp2z6"))

(define rust-zeroize-1.3.0
  (crate-source "zeroize" "1.3.0"
                "1z8yix823b6lz878qwg6bvwhg3lb0cbw3c9yij9p8mbv7zdzfmj7"))

(define pulseshitter-inputs
  (list rust-addr2line-0.21.0
        rust-adler-1.0.2
        rust-aead-0.4.3
        rust-aho-corasick-1.1.3
        rust-arrayvec-0.7.4
        rust-async-trait-0.1.80
        rust-async-tungstenite-0.17.2
        rust-atomic-waker-1.1.2
        rust-audiopus-0.3.0-rc.0
        rust-audiopus-sys-0.2.2
        rust-autocfg-1.3.0
        rust-backtrace-0.3.71
        rust-base64-0.13.1
        rust-base64-0.21.7
        rust-base64-0.22.1
        rust-bitflags-1.3.2
        rust-bitflags-2.5.0
        rust-block-buffer-0.10.4
        rust-bumpalo-3.16.0
        rust-bytemuck-1.16.0
        rust-byteorder-1.5.0
        rust-bytes-1.6.0
        rust-cassowary-0.3.0
        rust-cc-1.0.97
        rust-cfg-if-1.0.0
        rust-cipher-0.3.0
        rust-cmake-0.1.50
        rust-core-foundation-0.9.4
        rust-core-foundation-sys-0.8.6
        rust-cpufeatures-0.2.12
        rust-crc32fast-1.4.0
        rust-crossbeam-0.8.4
        rust-crossbeam-channel-0.5.12
        rust-crossbeam-deque-0.8.5
        rust-crossbeam-epoch-0.9.18
        rust-crossbeam-queue-0.3.11
        rust-crossbeam-utils-0.8.19
        rust-crossterm-0.25.0
        rust-crossterm-winapi-0.9.1
        rust-crypto-common-0.1.6
        rust-dashmap-5.5.3
        rust-deranged-0.3.11
        rust-derivative-2.2.0
        rust-digest-0.10.7
        rust-discortp-0.4.0
        rust-encoding-rs-0.8.34
        rust-enum-iterator-1.5.0
        rust-enum-iterator-derive-1.4.0
        rust-enum-primitive-0.1.1
        rust-equivalent-1.0.1
        rust-errno-0.3.9
        rust-fastrand-2.1.0
        rust-flate2-1.0.30
        rust-flume-0.10.14
        rust-fnv-1.0.7
        rust-foreign-types-0.3.2
        rust-foreign-types-shared-0.1.1
        rust-form-urlencoded-1.2.1
        rust-futures-0.3.30
        rust-futures-channel-0.3.30
        rust-futures-core-0.3.30
        rust-futures-executor-0.3.30
        rust-futures-io-0.3.30
        rust-futures-macro-0.3.30
        rust-futures-sink-0.3.30
        rust-futures-task-0.3.30
        rust-futures-util-0.3.30
        rust-generator-0.7.5
        rust-generic-array-0.14.7
        rust-getrandom-0.2.15
        rust-gimli-0.28.1
        rust-h2-0.3.26
        rust-h2-0.4.5
        rust-hashbrown-0.14.5
        rust-hermit-abi-0.3.9
        rust-http-0.2.12
        rust-http-1.1.0
        rust-http-body-0.4.6
        rust-http-body-1.0.0
        rust-http-body-util-0.1.1
        rust-httparse-1.8.0
        rust-httpdate-1.0.3
        rust-hyper-0.14.28
        rust-hyper-1.3.1
        rust-hyper-rustls-0.24.2
        rust-hyper-tls-0.6.0
        rust-hyper-util-0.1.5
        rust-idna-0.5.0
        rust-indexmap-2.2.6
        rust-ipnet-2.9.0
        rust-itoa-1.0.11
        rust-js-sys-0.3.69
        rust-lazy-static-1.4.0
        rust-libc-0.2.154
        rust-libpulse-binding-2.28.1
        rust-libpulse-sys-1.21.0
        rust-linux-raw-sys-0.4.14
        rust-lock-api-0.4.12
        rust-log-0.4.21
        rust-loom-0.5.6
        rust-matchers-0.1.0
        rust-memchr-2.7.2
        rust-mime-0.3.17
        rust-mime-guess-2.0.4
        rust-miniz-oxide-0.7.2
        rust-mio-0.8.11
        rust-multiversion-0.7.4
        rust-multiversion-macros-0.7.4
        rust-nanorand-0.7.0
        rust-native-tls-0.2.12
        rust-nu-ansi-term-0.46.0
        rust-num-conv-0.1.0
        rust-num-derive-0.3.3
        rust-num-traits-0.1.43
        rust-num-traits-0.2.19
        rust-num-cpus-1.16.0
        rust-object-0.32.2
        rust-once-cell-1.19.0
        rust-opaque-debug-0.3.1
        rust-openssl-0.10.64
        rust-openssl-macros-0.1.1
        rust-openssl-probe-0.1.5
        rust-openssl-sys-0.9.102
        rust-ordered-float-2.10.1
        rust-overload-0.1.1
        rust-parking-lot-0.12.2
        rust-parking-lot-core-0.9.10
        rust-percent-encoding-2.3.1
        rust-pin-project-1.1.5
        rust-pin-project-internal-1.1.5
        rust-pin-project-lite-0.2.14
        rust-pin-utils-0.1.0
        rust-pkg-config-0.3.30
        rust-pnet-base-0.28.0
        rust-pnet-macros-0.28.0
        rust-pnet-macros-support-0.28.0
        rust-poly1305-0.7.2
        rust-powerfmt-0.2.0
        rust-ppv-lite86-0.2.17
        rust-proc-macro2-1.0.82
        rust-quote-1.0.36
        rust-rand-0.8.5
        rust-rand-chacha-0.3.1
        rust-rand-core-0.6.4
        rust-redox-syscall-0.5.1
        rust-regex-1.10.4
        rust-regex-automata-0.1.10
        rust-regex-automata-0.4.6
        rust-regex-syntax-0.6.29
        rust-regex-syntax-0.8.3
        rust-reqwest-0.11.27
        rust-reqwest-0.12.4
        rust-ring-0.16.20
        rust-ring-0.17.8
        rust-ringbuf-0.3.3
        rust-ron-0.8.1
        rust-rustc-demangle-0.1.24
        rust-rustix-0.38.34
        rust-rustls-0.20.9
        rust-rustls-0.21.12
        rust-rustls-pemfile-1.0.4
        rust-rustls-pemfile-2.1.2
        rust-rustls-pki-types-1.7.0
        rust-rustls-webpki-0.101.7
        rust-rustversion-1.0.17
        rust-ryu-1.0.18
        rust-salsa20-0.9.0
        rust-schannel-0.1.23
        rust-scoped-tls-1.0.1
        rust-scopeguard-1.2.0
        rust-sct-0.7.1
        rust-security-framework-2.11.0
        rust-security-framework-sys-2.11.0
        rust-serde-1.0.201
        rust-serde-value-0.7.0
        rust-serde-derive-1.0.201
        rust-serde-json-1.0.117
        rust-serde-repr-0.1.19
        rust-serde-urlencoded-0.7.1
        rust-serenity-0.11.7
        rust-serenity-voice-model-0.1.1
        rust-sha-1-0.10.1
        rust-sharded-slab-0.1.7
        rust-signal-hook-0.3.17
        rust-signal-hook-mio-0.2.3
        rust-signal-hook-registry-1.4.2
        rust-slab-0.4.9
        rust-smallvec-1.13.2
        rust-socket2-0.5.7
        rust-songbird-0.3.2
        rust-spin-0.5.2
        rust-spin-0.9.8
        rust-streamcatcher-1.0.1
        rust-strsim-0.10.0
        rust-subtle-2.4.1
        rust-symphonia-core-0.5.4
        rust-syn-1.0.109
        rust-syn-2.0.63
        rust-sync-wrapper-0.1.2
        rust-system-configuration-0.5.1
        rust-system-configuration-sys-0.5.0
        rust-target-features-0.1.6
        rust-tempfile-3.10.1
        rust-thiserror-1.0.60
        rust-thiserror-impl-1.0.60
        rust-thread-local-1.1.8
        rust-time-0.3.36
        rust-time-core-0.1.2
        rust-time-macros-0.2.18
        rust-tinyvec-1.6.0
        rust-tinyvec-macros-0.1.1
        rust-tokio-1.37.0
        rust-tokio-macros-2.2.0
        rust-tokio-native-tls-0.3.1
        rust-tokio-rustls-0.23.4
        rust-tokio-rustls-0.24.1
        rust-tokio-util-0.7.11
        rust-tower-0.4.13
        rust-tower-layer-0.3.2
        rust-tower-service-0.3.2
        rust-tracing-0.1.40
        rust-tracing-attributes-0.1.27
        rust-tracing-core-0.1.32
        rust-tracing-futures-0.2.5
        rust-tracing-log-0.2.0
        rust-tracing-subscriber-0.3.18
        rust-try-lock-0.2.5
        rust-tui-0.19.0
        rust-tui-textarea-0.2.4
        rust-tungstenite-0.17.3
        rust-typemap-rev-0.1.5
        rust-typenum-1.17.0
        rust-unicase-2.7.0
        rust-unicode-bidi-0.3.15
        rust-unicode-ident-1.0.12
        rust-unicode-normalization-0.1.23
        rust-unicode-segmentation-1.11.0
        rust-unicode-width-0.1.12
        rust-universal-hash-0.4.1
        rust-untrusted-0.7.1
        rust-untrusted-0.9.0
        rust-url-2.5.0
        rust-utf-8-0.7.6
        rust-uuid-0.8.2
        rust-valuable-0.1.0
        rust-vcpkg-0.2.15
        rust-version-check-0.9.4
        rust-want-0.3.1
        rust-wasi-0.11.0+wasi-snapshot-preview1
        rust-wasm-bindgen-0.2.92
        rust-wasm-bindgen-backend-0.2.92
        rust-wasm-bindgen-futures-0.4.42
        rust-wasm-bindgen-macro-0.2.92
        rust-wasm-bindgen-macro-support-0.2.92
        rust-wasm-bindgen-shared-0.2.92
        rust-wasm-streams-0.4.0
        rust-web-sys-0.3.69
        rust-webpki-0.22.4
        rust-webpki-roots-0.22.6
        rust-webpki-roots-0.25.4
        rust-winapi-0.3.9
        rust-winapi-i686-pc-windows-gnu-0.4.0
        rust-winapi-x86-64-pc-windows-gnu-0.4.0
        rust-windows-0.48.0
        rust-windows-sys-0.48.0
        rust-windows-sys-0.52.0
        rust-windows-targets-0.48.5
        rust-windows-targets-0.52.5
        rust-windows-aarch64-gnullvm-0.48.5
        rust-windows-aarch64-gnullvm-0.52.5
        rust-windows-aarch64-msvc-0.48.5
        rust-windows-aarch64-msvc-0.52.5
        rust-windows-i686-gnu-0.48.5
        rust-windows-i686-gnu-0.52.5
        rust-windows-i686-gnullvm-0.52.5
        rust-windows-i686-msvc-0.48.5
        rust-windows-i686-msvc-0.52.5
        rust-windows-x86-64-gnu-0.48.5
        rust-windows-x86-64-gnu-0.52.5
        rust-windows-x86-64-gnullvm-0.48.5
        rust-windows-x86-64-gnullvm-0.52.5
        rust-windows-x86-64-msvc-0.48.5
        rust-windows-x86-64-msvc-0.52.5
        rust-winreg-0.50.0
        rust-winreg-0.52.0
        rust-xsalsa20poly1305-0.8.0
        rust-zeroize-1.3.0))

(define-public pulseshitter
  (package
    (name "pulseshitter")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Enitoni/pulseshitter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lwpgqsz907n6l5ff92g8q3sy1i7bf7j1s0yiq7lrrzdshz6xs4m"))))
    (build-system cargo-build-system)
    (arguments (list #:install-source? #f))

    (native-inputs (list pkg-config cmake))
    (inputs (cons* pulseaudio opus openssl
                   pulseshitter-inputs))
    (home-page "https://githubcom/Enitoni/pulseshitter/tree/main")
    (synopsis "Shit audio into a Discord bot from Pulse or Pipewire")
    (description synopsis)
    (license (list license:mpl2.0))))
pulseshitter
