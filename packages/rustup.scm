(define-module (packages rustup)
  #:use-module (guix build-system cargo)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:export (rustup lookup-cargo-inputs))

(define rust-adler2-2.0.1
  (crate-source "adler2" "2.0.1"
                "1ymy18s9hs7ya1pjc9864l30wk8p2qfqdi7mhhcc5nfakxbij09j"))

(define rust-aho-corasick-1.1.4
  (crate-source "aho-corasick" "1.1.4"
                "00a32wb2h07im3skkikc495jvncf62jl6s96vwc7bhi70h9imlyx"))

(define rust-aligned-0.4.3
  (crate-source "aligned" "0.4.3"
                "1186lhb3gb4x6spzw7ff0zcraa8cr9zqk4ldpm5g1vb2ijc0higf"))

(define rust-anstream-0.6.21
  (crate-source "anstream" "0.6.21"
                "0jjgixms4qjj58dzr846h2s29p8w7ynwr9b9x6246m1pwy0v5ma3"))

(define rust-anstream-1.0.0
  (crate-source "anstream" "1.0.0"
                "13d2bj0xfg012s4rmq44zc8zgy1q8k9yp7yhvfnarscnmwpj2jl2"))

(define rust-anstyle-1.0.13
  (crate-source "anstyle" "1.0.13"
                "0y2ynjqajpny6q0amvfzzgw0gfw3l47z85km4gvx87vg02lcr4ji"))

(define rust-anstyle-1.0.14
  (crate-source "anstyle" "1.0.14"
                "0030szmgj51fxkic1hpakxxgappxzwm6m154a3gfml83lq63l2wl"))

(define rust-anstyle-lossy-1.1.4
  (crate-source "anstyle-lossy" "1.1.4"
                "07x0kqkklc0124cbn49fc21d9wzp9w2vhaw827md113ghbfablq4"))

(define rust-anstyle-lossy-1.1.5
  (crate-source "anstyle-lossy" "1.1.5"
                "1ymyqn1sgf5q8vhcm0vpbv3k1inpbwnhn3cp2zcddz0aa87pvjnr"))

(define rust-anstyle-parse-0.2.7
  (crate-source "anstyle-parse" "0.2.7"
                "1hhmkkfr95d462b3zf6yl2vfzdqfy5726ya572wwg8ha9y148xjf"))

(define rust-anstyle-parse-1.0.0
  (crate-source "anstyle-parse" "1.0.0"
                "03hkv2690s0crssbnmfkr76kw1k7ah2i6s5amdy9yca2n8w7zkjj"))

(define rust-anstyle-query-1.1.5
  (crate-source "anstyle-query" "1.1.5"
                "1p6shfpnbghs6jsa0vnqd8bb8gd7pjd0jr7w0j8jikakzmr8zi20"))

(define rust-anstyle-svg-0.1.12
  (crate-source "anstyle-svg" "0.1.12"
                "0qj39pf4cf86207a89vgdl6ij7izw81h43xxfmyakgcbx8yrybg2"))

(define rust-anstyle-svg-1.1.0
  (crate-source "anstyle-svg" "1.1.0"
                "11dzzw7xshc6c8017j9bjmrsdc5fch3hn4bb7wwkf27wi3xcaznv"))

(define rust-anstyle-wincon-3.0.11
  (crate-source "anstyle-wincon" "3.0.11"
                "0zblannm70sk3xny337mz7c6d8q8i24vhbqi42ld8v7q1wjnl7i9"))

(define rust-anyhow-1.0.102
  (crate-source "anyhow" "1.0.102"
                "0b447dra1v12z474c6z4jmicdmc5yxz5bakympdnij44ckw2s83z"))

(define rust-as-slice-0.2.1
  (crate-source "as-slice" "0.2.1"
                "05j52y1ws8kir5zjxnl48ann0if79sb56p9nm76hvma01r7nnssi"))

(define rust-async-compression-0.4.41
  (crate-source "async-compression" "0.4.41"
                "1lfs8dy6kla1bi8kn4h1zppv79zvk524ds8nb2nxgzq2dq7yxyfh"))

(define rust-async-trait-0.1.89
  (crate-source "async-trait" "0.1.89"
                "1fsxxmz3rzx1prn1h3rs7kyjhkap60i7xvi0ldapkvbb14nssdch"))

(define rust-atomic-waker-1.1.2
  (crate-source "atomic-waker" "1.1.2"
                "1h5av1lw56m0jf0fd3bchxq8a30xv0b4wv8s4zkp4s0i7mfvs18m"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-aws-lc-rs-1.16.1
  (crate-source "aws-lc-rs" "1.16.1"
                "1gzlb3c82vv3b9adi15kqpk8wps699rjssc3ijkc42pidl0grgwl"))

(define rust-aws-lc-rs-1.16.2
  (crate-source "aws-lc-rs" "1.16.2"
                "1z6i8qs0xjnzvslxnkhvywzzwfkafb1s4nrpg3f2k1nii4i92m50"))

(define rust-aws-lc-sys-0.38.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "aws-lc-sys" "0.38.0"
                "0bkqm9adn7f8c8hd3dnp16cgh39cgjckfzqs55ymmfw9xmlfa8a3"))

(define rust-aws-lc-sys-0.39.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "aws-lc-sys" "0.39.0"
                "02jga4605vwqcxzd4k3ikd01x27k4gqwd8hh2rs7qm2w9hmfb9qz"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

(define rust-bit-set-0.8.0
  (crate-source "bit-set" "0.8.0"
                "18riaa10s6n59n39vix0cr7l2dgwdhcpbcm97x1xbyfp1q47x008"))

(define rust-bit-vec-0.8.0
  (crate-source "bit-vec" "0.8.0"
                "1xxa1s2cj291r7k1whbxq840jxvmdsq9xgh7bvrxl46m80fllxjy"))

(define rust-bitflags-2.11.0
  (crate-source "bitflags" "2.11.0"
                "1bwjibwry5nfwsfm9kjg2dqx5n5nja9xymwbfl6svnn8jsz6ff44"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-bstr-1.12.1
  (crate-source "bstr" "1.12.1"
                "1arc1v7h5l86vd6z76z3xykjzldqd5icldn7j9d3p7z6x0d4w133"))

(define rust-bumpalo-3.20.2
  (crate-source "bumpalo" "3.20.2"
                "1jrgxlff76k9glam0akhwpil2fr1w32gbjdf5hpipc7ld2c7h82x"))

(define rust-bytes-1.11.1
  (crate-source "bytes" "1.11.1"
                "0czwlhbq8z29wq0ia87yass2mzy1y0jcasjb8ghriiybnwrqfx0y"))

(define rust-cc-1.2.56
  (crate-source "cc" "1.2.56"
                "1chvh9g2izhqad7vzy4cc7xpdljdvqpsr6x6hv1hmyqv3mlkbgxf"))

(define rust-cc-1.2.57
  (crate-source "cc" "1.2.57"
                "08q464b62d03zm7rgiixavkrh5lzfq18lwf884vgycj9735d23bs"))

(define rust-cesu8-1.1.0
  (crate-source "cesu8" "1.1.0"
                "0g6q58wa7khxrxcxgnqyi9s1z2cjywwwd3hzr5c55wskhx6s0hvd"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-if-0.1.10
  (crate-source "cfg-if" "0.1.10"
                "08h80ihs74jcyp24cd75wwabygbbdgl05k6p5dmq8akbr78vv1a7"))

(define rust-cfg-if-1.0.4
  (crate-source "cfg-if" "1.0.4"
                "008q28ajc546z5p2hcwdnckmg0hia7rnx52fni04bwqkzyrghc4k"))

(define rust-chacha20-0.10.0
  (crate-source "chacha20" "0.10.0"
                "00bn2rn8l68qvlq93mhq7b4ns4zy9qbjsyjbb9kljgl4hqr9i3bg"))

(define rust-chrono-0.4.44
  (crate-source "chrono" "0.4.44"
                "1c64mk9a235271j5g3v4zrzqqmd43vp9vki7vqfllpqf5rd0fwy6"))

(define rust-clap-4.5.60
  (crate-source "clap" "4.5.60"
                "02h3nzznssjgp815nnbzk0r62y2iw03kdli75c233kirld6z75r7"))

(define rust-clap-4.6.0
  (crate-source "clap" "4.6.0"
                "0l8k0ja5rf4hpn2g98bqv5m6lkh2q6b6likjpmm6fjw3cxdsz4xi"))

(define rust-clap-builder-4.5.60
  (crate-source "clap_builder" "4.5.60"
                "0xk8mdizvmmn6w5ij5cwhy5pbgyac4w9pfvl6nqmjl7a5hql38i4"))

(define rust-clap-builder-4.6.0
  (crate-source "clap_builder" "4.6.0"
                "17q6np22yxhh5y5v53y4l31ps3hlaz45mvz2n2nicr7n3c056jki"))

(define rust-clap-cargo-0.18.3
  (crate-source "clap-cargo" "0.18.3"
                "01l070c3a88spywm8drhfzwn3ljp142fqsi1p15pan42bj9m2rck"))

(define rust-clap-complete-4.5.66
  (crate-source "clap_complete" "4.5.66"
                "0c8h6x3x1ddldfmhii12hrd92v1av8d18rckdzjs8qciwfvs6my7"))

(define rust-clap-complete-4.6.0
  (crate-source "clap_complete" "4.6.0"
                "1sric3allwj3isk81khgmsx9qab1bafyr37jh4v6wwvbwzfz3j8r"))

(define rust-clap-derive-4.5.55
  (crate-source "clap_derive" "4.5.55"
                "1r949xis3jmhzh387smd70vc8a3b9734ck3g5ahg59a63bd969x9"))

(define rust-clap-derive-4.6.0
  (crate-source "clap_derive" "4.6.0"
                "0snapc468s7n3avr33dky4y7rmb7ha3qsp9l0k5vh6jacf5bs40i"))

(define rust-clap-lex-1.0.0
  (crate-source "clap_lex" "1.0.0"
                "0c8888qi1l9sayqlv666h8s0yxn2qc6jr88v1zagk43mpjjjx0is"))

(define rust-clap-lex-1.1.0
  (crate-source "clap_lex" "1.1.0"
                "1ycqkpygnlqnndghhcxjb44lzl0nmgsia64x9581030yifxs7m68"))

(define rust-cmake-0.1.57
  (crate-source "cmake" "0.1.57"
                "0zgg10qgykig4nxyf7whrqfg7fkk0xfxhiavikmrndvbrm23qi3m"))

(define rust-colorchoice-1.0.4
  (crate-source "colorchoice" "1.0.4"
                "0x8ymkz1xr77rcj1cfanhf416pc4v681gmkc9dzb3jqja7f62nxh"))

(define rust-colorchoice-1.0.5
  (crate-source "colorchoice" "1.0.5"
                "0w75k89hw39p0mnnhlrwr23q50rza1yjki44qvh2mgrnj065a1qx"))

(define rust-combine-4.6.7
  (crate-source "combine" "4.6.7"
                "1z8rh8wp59gf8k23ar010phgs0wgf5i8cx4fg01gwcnzfn5k0nms"))

(define rust-compression-codecs-0.4.37
  (crate-source "compression-codecs" "0.4.37"
                "1dqrcv8myady3z5qgj351l6ca68g2n3blq9hfwkgqry9v6km2yzb"))

(define rust-compression-core-0.4.31
  (crate-source "compression-core" "0.4.31"
                "13cxnh46qvli55aqv04i3l6kiw2835ngp6mr5paa00nidvxlx63m"))

(define rust-console-0.16.2
  (crate-source "console" "0.16.2"
                "1i5y6h3myz38jl9p3gglx5vh9c69kxxajsv3jx0pw8i6i555mr03"))

(define rust-console-0.16.3
  (crate-source "console" "0.16.3"
                "11zwz1vnfr0nx6dyjx0gjymp8864y5hxwf01ynfd2s8kapsqlknn"))

(define rust-core-foundation-0.10.1
  (crate-source "core-foundation" "0.10.1"
                "1xjns6dqf36rni2x9f47b65grxwdm20kwdg9lhmzdrrkwadcv9mj"))

(define rust-core-foundation-sys-0.8.7
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-cpufeatures-0.2.17
  (crate-source "cpufeatures" "0.2.17"
                "10023dnnaghhdl70xcds12fsx2b966sxbxjq5sxs49mvxqw5ivar"))

(define rust-cpufeatures-0.3.0
  (crate-source "cpufeatures" "0.3.0"
                "00fjhygsqmh4kbxxlb99mcsbspxcai6hjydv4c46pwb67wwl2alb"))

(define rust-crc32fast-1.5.0
  (crate-source "crc32fast" "1.5.0"
                "04d51liy8rbssra92p0qnwjw8i9rm9c4m3bwy19wjamz1k4w30cl"))

(define rust-crossbeam-deque-0.8.6
  (crate-source "crossbeam-deque" "0.8.6"
                "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-crypto-common-0.1.7
  (crate-source "crypto-common" "0.1.7"
                "02nn2rhfy7kvdkdjl457q2z0mklcvj9h662xrq6dzhfialh2kj3q"))

(define rust-curl-0.4.49
  (crate-source "curl" "0.4.49"
                "1g7dcrh4mwkn2q0iiga7vc8i68pd5jdby5aparpa6yxqs1nkpz3r"))

(define rust-curl-sys-0.4.85+curl-8.18.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "curl-sys" "0.4.85+curl-8.18.0"
                "1nrzryl8hw1br69bagr774wrv2w8yim9x8zasgv0bk2y5caadvy0"))

(define rust-curl-sys-0.4.86+curl-8.19.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "curl-sys" "0.4.86+curl-8.19.0"
                "0i8703gh9sgwmnfhp4zdqg4vgpm25aw385l01p734ifghyjdc79s"))

(define rust-cvt-0.1.2
  (crate-source "cvt" "0.1.2"
                "1wdwg2rbjl86bcrymscl34pw31kyv1ada19jazpkjbdzgzvrpbnj"))

(define rust-deranged-0.5.8
  (crate-source "deranged" "0.5.8"
                "0711df3w16vx80k55ivkwzwswziinj4dz05xci3rvmn15g615n3w"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

(define rust-dunce-1.0.5
  (crate-source "dunce" "1.0.5"
                "04y8wwv3vvcqaqmqzssi6k0ii9gs6fpz96j5w9nky2ccsl23axwj"))

(define rust-effective-limits-0.5.5
  (crate-source "effective-limits" "0.5.5"
                "01wswblavql8gyc9dkibcfab56wjc4mmng53kg4jnjs6lw0my69p"))

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-encode-unicode-1.0.0
  (crate-source "encode_unicode" "1.0.0"
                "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail"))

(define rust-enum-map-2.7.3
  (crate-source "enum-map" "2.7.3"
                "1sgjgl4mmz93jdkfdsmapc3dmaq8gddagw9s0fd501w2vyzz6rk8"))

(define rust-enum-map-derive-0.17.0
  (crate-source "enum-map-derive" "0.17.0"
                "1sv4mb343rsz4lc3rh7cyn0pdhf7fk18k1dgq8kfn5i5x7gwz0pj"))

(define rust-env-proxy-0.4.1
  (crate-source "env_proxy" "0.4.1"
                "1qabqhgybx1jzh6dmpx7kssciw312i8aa6al7fj0d12k32z1jl1s"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-errno-0.3.14
  (crate-source "errno" "0.3.14"
                "1szgccmh8vgryqyadg8xd58mnwwicf39zmin3bsn63df2wbbgjir"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-filetime-0.2.27
  (crate-source "filetime" "0.2.27"
                "1nspbkm1d1km7xfljcbl565swqxrihqyin8bqppig2gf3qal927r"))

(define rust-find-msvc-tools-0.1.9
  (crate-source "find-msvc-tools" "0.1.9"
                "10nmi0qdskq6l7zwxw5g56xny7hb624iki1c39d907qmfh3vrbjv"))

(define rust-flate2-1.1.9
  (crate-source "flate2" "1.1.9"
                "0g2pb7cxnzcbzrj8bw4v6gpqqp21aycmf6d84rzb6j748qkvlgw4"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-foldhash-0.1.5
  (crate-source "foldhash" "0.1.5"
                "1wisr1xlc2bj7hk4rgkcjkz3j2x4dhd1h9lwk7mj8p71qpdgbi6r"))

(define rust-foreign-types-0.3.2
  (crate-source "foreign-types" "0.3.2"
                "1cgk0vyd7r45cj769jym4a6s7vwshvd0z4bqrb92q1fwibmkkwzn"))

(define rust-foreign-types-shared-0.1.1
  (crate-source "foreign-types-shared" "0.1.1"
                "0jxgzd04ra4imjv8jgkmdq59kj8fsz6w4zxsbmlai34h26225c00"))

(define rust-form-urlencoded-1.2.2
  (crate-source "form_urlencoded" "1.2.2"
                "1kqzb2qn608rxl3dws04zahcklpplkd5r1vpabwga5l50d2v4k6b"))

(define rust-fs-at-0.2.1
  (crate-source "fs_at" "0.2.1"
                "0dn0hi9inmppk3mypvnaimjcdrxr0f3pi8d2p8jxn9gajjb6rbql"))

(define rust-fs-extra-1.3.0
  (crate-source "fs_extra" "1.3.0"
                "075i25z70j2mz9r7i9p9r521y8xdj81q7skslyb7zhqnnw33fw22"))

(define rust-futures-channel-0.3.32
  (crate-source "futures-channel" "0.3.32"
                "07fcyzrmbmh7fh4ainilf1s7gnwvnk07phdq77jkb9fpa2ffifq7"))

(define rust-futures-core-0.3.32
  (crate-source "futures-core" "0.3.32"
                "07bbvwjbm5g2i330nyr1kcvjapkmdqzl4r6mqv75ivvjaa0m0d3y"))

(define rust-futures-executor-0.3.32
  (crate-source "futures-executor" "0.3.32"
                "17aplz3ns74qn7a04qg7qlgsdx5iwwwkd4jvdfra6hl3h4w9rwms"))

(define rust-futures-io-0.3.32
  (crate-source "futures-io" "0.3.32"
                "063pf5m6vfmyxj74447x8kx9q8zj6m9daamj4hvf49yrg9fs7jyf"))

(define rust-futures-macro-0.3.32
  (crate-source "futures-macro" "0.3.32"
                "0ys4b1lk7s0bsj29pv42bxsaavalch35rprp64s964p40c1bfdg8"))

(define rust-futures-sink-0.3.32
  (crate-source "futures-sink" "0.3.32"
                "14q8ml7hn5a6gyy9ri236j28kh0svqmrk4gcg0wh26rkazhm95y3"))

(define rust-futures-task-0.3.32
  (crate-source "futures-task" "0.3.32"
                "14s3vqf8llz3kjza33vn4ixg6kwxp61xrysn716h0cwwsnri2xq3"))

(define rust-futures-util-0.3.32
  (crate-source "futures-util" "0.3.32"
                "1mn60lw5kh32hz9isinjlpw34zx708fk5q1x0m40n6g6jq9a971q"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-getrandom-0.2.17
  (crate-source "getrandom" "0.2.17"
                "1l2ac6jfj9xhpjjgmcx6s1x89bbnw9x6j9258yy6xjkzpq0bqapz"))

(define rust-getrandom-0.3.4
  (crate-source "getrandom" "0.3.4"
                "1zbpvpicry9lrbjmkd4msgj3ihff1q92i334chk7pzf46xffz7c9"))

(define rust-getrandom-0.4.1
  (crate-source "getrandom" "0.4.1"
                "1v7fm84f2jh6x7w3bd2ncl3sw29wnb0rhg7xya1pd30i02cg77hk"))

(define rust-getrandom-0.4.2
  (crate-source "getrandom" "0.4.2"
                "0mb5833hf9pvn9dhvxjgfg5dx0m77g8wavvjdpvpnkp9fil1xr8d"))

(define rust-git-testament-0.2.6
  (crate-source "git-testament" "0.2.6"
                "0l9f4d4p21lfvx25640q7xl1i6m749ljx7bsr0czjy8ljaf9jx2s"))

(define rust-git-testament-derive-0.2.1
  (crate-source "git-testament-derive" "0.2.1"
                "1h645dqdzzgibgkar3vki1mkpkf73r9cfbvlaqbdrcqywxkwksmv"))

(define rust-h2-0.4.13
  (crate-source "h2" "0.4.13"
                "0m6w5gg0n0m1m5915bxrv8n4rlazhx5icknkslz719jhh4xdli1g"))

(define rust-hashbrown-0.15.5
  (crate-source "hashbrown" "0.15.5"
                "189qaczmjxnikm9db748xyhiw04kpmhm9xj9k9hg0sgx7pjwyacj"))

(define rust-hashbrown-0.16.1
  (crate-source "hashbrown" "0.16.1"
                "004i3njw38ji3bzdp9z178ba9x3k0c1pgy8x69pj7yfppv4iq7c4"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hermit-abi-0.5.2
  (crate-source "hermit-abi" "0.5.2"
                "1744vaqkczpwncfy960j2hxrbjl1q01csm84jpd9dajbdr2yy3zw"))

(define rust-home-0.5.12
  (crate-source "home" "0.5.12"
                "13bjyzgx6q9srnfvl43dvmhn93qc8mh5w7cylk2g13sj3i3pyqnc"))

(define rust-html-escape-0.2.13
  (crate-source "html-escape" "0.2.13"
                "0xml3hswv0205fbm5iq7dqiwjkr6d245xkfppwi7wqjdfr4x86kd"))

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

(define rust-httpdate-1.0.3
  (crate-source "httpdate" "1.0.3"
                "1aa9rd2sac0zhjqh24c9xvir96g188zldkx0hr6dnnlx5904cfyz"))

(define rust-hyper-1.8.1
  (crate-source "hyper" "1.8.1"
                "04cxr8j5y86bhxxlyqb8xkxjskpajk7cxwfzzk4v3my3a3rd9cia"))

(define rust-hyper-rustls-0.27.7
  (crate-source "hyper-rustls" "0.27.7"
                "0n6g8998szbzhnvcs1b7ibn745grxiqmlpg53xz206v826v3xjg3"))

(define rust-hyper-timeout-0.5.2
  (crate-source "hyper-timeout" "0.5.2"
                "1c431l5ckr698248yd6bnsmizjy2m1da02cbpmsnmkpvpxkdb41b"))

(define rust-hyper-tls-0.6.0
  (crate-source "hyper-tls" "0.6.0"
                "1q36x2yps6hhvxq5r7mc8ph9zz6xlb573gx0x3yskb0fi736y83h"))

(define rust-hyper-util-0.1.20
  (crate-source "hyper-util" "0.1.20"
                "186zdc58hmm663csmjvrzgkr6jdh93sfmi3q2pxi57gcaqjpqm4n"))

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

(define rust-idna-1.1.0
  (crate-source "idna" "1.1.0"
                "1pp4n7hppm480zcx411dsv9wfibai00wbpgnjj4qj0xa7kr7a21v"))

(define rust-idna-adapter-1.2.1
  (crate-source "idna_adapter" "1.2.1"
                "0i0339pxig6mv786nkqcxnwqa87v4m94b2653f6k3aj0jmhfkjis"))

(define rust-indexmap-2.13.0
  (crate-source "indexmap" "2.13.0"
                "05qh5c4h2hrnyypphxpwflk45syqbzvqsvvyxg43mp576w2ff53p"))

(define rust-indicatif-0.18.4
  (crate-source "indicatif" "0.18.4"
                "1sz9p1a7i0z666psqzjdpi8xa11icmnpfd4q4dyxm4ihh0ihyir5"))

(define rust-ipnet-2.11.0
  (crate-source "ipnet" "2.11.0"
                "0c5i9sfi2asai28m8xp48k5gvwkqrg5ffpi767py6mzsrswv17s6"))

(define rust-ipnet-2.12.0
  (crate-source "ipnet" "2.12.0"
                "1qpq2y0asyv0jppw7zww9y96fpnpinwap8a0phhqqgyy3znnz3yr"))

(define rust-iri-string-0.7.10
  (crate-source "iri-string" "0.7.10"
                "06kk3a5jz576p7vrpf7zz9jv3lrgcyp7pczcblcxdnryg3q3h4y9"))

(define rust-is-terminal-polyfill-1.70.2
  (crate-source "is_terminal_polyfill" "1.70.2"
                "15anlc47sbz0jfs9q8fhwf0h3vs2w4imc030shdnq54sny5i7jx6"))

(define rust-itertools-0.14.0
  (crate-source "itertools" "0.14.0"
                "118j6l1vs2mx65dqhwyssbrxpawa90886m3mzafdvyip41w2q69b"))

(define rust-itoa-1.0.17
  (crate-source "itoa" "1.0.17"
                "1lh93xydrdn1g9x547bd05g0d3hra7pd1k4jfd2z1pl1h5hwdv4j"))

(define rust-itoa-1.0.18
  (crate-source "itoa" "1.0.18"
                "10jnd1vpfkb8kj38rlkn2a6k02afvj3qmw054dfpzagrpl6achlg"))

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

(define rust-js-sys-0.3.91
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "js-sys" "0.3.91"
                "171rzgq33wc1nxkgnvhlqqwwnrifs13mg3jjpjj5nf1z0yvib5xl"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-leb128fmt-0.1.0
  (crate-source "leb128fmt" "0.1.0"
                "1chxm1484a0bly6anh6bd7a99sn355ymlagnwj3yajafnpldkv89"))

(define rust-libc-0.2.182
  (crate-source "libc" "0.2.182"
                "04k1w1mq9f4cxv520dbr5xw1i7xkbc9fcrvaggyjy25jdkdvl038"))

(define rust-libc-0.2.183
  (crate-source "libc" "0.2.183"
                "17c9gyia7rrzf9gsssvk3vq9ca2jp6rh32fsw6ciarpn5djlddmm"))

(define rust-libredox-0.1.14
  (crate-source "libredox" "0.1.14"
                "02p3pxlqf54znf1jhiyyjs0i4caf8ckrd5l8ygs4i6ba3nfy6i0p"))

(define rust-libz-sys-1.1.24
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "libz-sys" "1.1.24"
                "0f8879301wxgljw8snkcix90p6qbm4inp3sqrsjq9b2svv5yjda7"))

(define rust-linux-raw-sys-0.12.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.12.1"
                "0lwasljrqxjjfk9l2j8lyib1babh2qjlnhylqzl01nihw14nk9ij"))

(define rust-litemap-0.8.1
  (crate-source "litemap" "0.8.1"
                "0xsy8pfp9s802rsj1bq2ys2kbk1g36w5dr3gkfip7gphb5x60wv3"))

(define rust-log-0.4.29
  (crate-source "log" "0.4.29"
                "15q8j9c8g5zpkcw0hnd6cf2z7fxqnvsjh3rw5mv5q10r83i34l2y"))

(define rust-lzma-sys-0.1.20
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "lzma-sys" "0.1.20"
                "09sxp20waxyglgn3cjz8qjkspb3ryz2fwx4rigkwvrk46ymh9njz"))

(define rust-matchers-0.1.0
  (crate-source "matchers" "0.1.0"
                "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2"))

(define rust-matchers-0.2.0
  (crate-source "matchers" "0.2.0"
                "1sasssspdj2vwcwmbq3ra18d3qniapkimfcbr47zmx6750m5llni"))

(define rust-memchr-2.8.0
  (crate-source "memchr" "2.8.0"
                "0y9zzxcqxvdqg6wyag7vc3h0blhdn7hkq164bxyx2vph8zs5ijpq"))

(define rust-miniz-oxide-0.8.9
  (crate-source "miniz_oxide" "0.8.9"
                "05k3pdg8bjjzayq3rf0qhpirq9k37pxnasfn4arbs17phqn6m9qz"))

(define rust-mio-1.1.1
  (crate-source "mio" "1.1.1"
                "1z2phpalqbdgihrcjp8y09l3kgq6309jnhnr6h11l9s7mnqcm6x6"))

(define rust-native-tls-0.2.18
  (crate-source "native-tls" "0.2.18"
                "1wmv0g5p6jwyyslyw88w5fv9kc9qvjd1hi2d4sfl4qm19vhh0ma6"))

(define rust-nix-0.29.0
  (crate-source "nix" "0.29.0"
                "0ikvn7s9r2lrfdm3mx1h7nbfjvcc6s9vxdzw7j5xfkd2qdnp9qki"))

(define rust-normalize-line-endings-0.3.0
  (crate-source "normalize-line-endings" "0.3.0"
                "1gp52dfn2glz26a352zra8h04351icf0fkqzw1shkwrgh1vpz031"))

(define rust-normpath-1.5.0
  (crate-source "normpath" "1.5.0"
                "16z68q809749ky2vl72f3lqnhf3vjclvcc3y2z5v8m2nj0msn8xz"))

(define rust-nu-ansi-term-0.46.0
  (crate-source "nu-ansi-term" "0.46.0"
                "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))

(define rust-nu-ansi-term-0.50.3
  (crate-source "nu-ansi-term" "0.50.3"
                "1ra088d885lbd21q1bxgpqdlk1zlndblmarn948jz2a40xsbjmvr"))

(define rust-num-conv-0.2.0
  (crate-source "num-conv" "0.2.0"
                "0l4hj7lp8zbb9am4j3p7vlcv47y9bbazinvnxx9zjhiwkibyr5yg"))

(define rust-num-cpus-1.17.0
  (crate-source "num_cpus" "1.17.0"
                "0fxjazlng4z8cgbmsvbzv411wrg7x3hyxdq8nxixgzjswyylppwi"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-once-cell-1.21.4
  (crate-source "once_cell" "1.21.4"
                "0l1v676wf71kjg2khch4dphwh1jp3291ffiymr2mvy1kxd5kwz4z"))

(define rust-once-cell-polyfill-1.70.2
  (crate-source "once_cell_polyfill" "1.70.2"
                "1zmla628f0sk3fhjdjqzgxhalr2xrfna958s632z65bjsfv8ljrq"))

(define rust-opener-0.8.4
  (crate-source "opener" "0.8.4"
                "1vn20ka1zkrzl2z71hazyy9b2giky66i1p7i7v0mfczi1iz37ym2"))

(define rust-openssl-0.10.75
  (crate-source "openssl" "0.10.75"
                "0a238gvrzjq0r62a7472i685hi5jjzgfj72kp1xd32ir46qqv0q8"))

(define rust-openssl-0.10.76
  (crate-source "openssl" "0.10.76"
                "1kwfn77qi342fr3hn1kxza6hslyma4ylszlcbg3a4vp1fln0074m"))

(define rust-openssl-macros-0.1.1
  (crate-source "openssl-macros" "0.1.1"
                "173xxvfc63rr5ybwqwylsir0vq6xsj4kxiv4hmg4c3vscdmncj59"))

(define rust-openssl-probe-0.1.6
  (crate-source "openssl-probe" "0.1.6"
                "0bl52x55laalqb707k009h8kfawliwp992rlsvkzy49n47p2fpnh"))

(define rust-openssl-probe-0.2.1
  (crate-source "openssl-probe" "0.2.1"
                "1gpwpb7smfhkscwvbri8xzbab39wcnby1jgz1s49vf1aqgsdx1vw"))

(define rust-openssl-src-300.5.4+3.5.4
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "openssl-src" "300.5.4+3.5.4"
                "0wnbqw38pzp66axaw2wz5my8nhg8f4viw74avyqfknlm55wv61x5"))

(define rust-openssl-sys-0.9.111
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "openssl-sys" "0.9.111"
                "08f3mpsabivfi3fd0qv9231qidqy68lr8a4qi32y6xda43av5jl2"))

(define rust-openssl-sys-0.9.112
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "openssl-sys" "0.9.112"
                "1nyvn3nvkcknjpkh8c19zjb9q3mbcyxgsvajw4mm09p2ngrmmmap"))

(define rust-opentelemetry-0.31.0
  (crate-source "opentelemetry" "0.31.0"
                "18629xsj4rsyiby9aj511q6wcw6s9m09gx3ymw1yjcvix1mcsjxq"))

(define rust-opentelemetry-http-0.31.0
  (crate-source "opentelemetry-http" "0.31.0"
                "0pc5nw1ds8v8w0nvyall39m92v8m1xl1p3vwvxk6nkhrffdd19np"))

(define rust-opentelemetry-otlp-0.31.0
  (crate-source "opentelemetry-otlp" "0.31.0"
                "1gv3h75z8c0p9b85mbq7f1rgsi18wip1xlfa6g82lkfa5pdnc8vs"))

(define rust-opentelemetry-otlp-0.31.1
  (crate-source "opentelemetry-otlp" "0.31.1"
                "07zp0b62b9dajnvvcd6j2ppw5zg7wp4ixka9z6fr3bxrrdmcss8z"))

(define rust-opentelemetry-proto-0.31.0
  (crate-source "opentelemetry-proto" "0.31.0"
                "03xkjsjrsm7zkkx5gascqd9bg2z20wymm06l16cyxsp5dpq5s5x7"))

(define rust-opentelemetry-sdk-0.31.0
  (crate-source "opentelemetry_sdk" "0.31.0"
                "1gbjsggdxfpjbanjvaxa3nq32vfa37i3v13dvx4gsxhrk7sy8jp1"))

(define rust-overload-0.1.1
  (crate-source "overload" "0.1.1"
                "0fdgbaqwknillagy1xq7xfgv60qdbk010diwl7s1p0qx7hb16n5i"))

(define rust-percent-encoding-2.3.2
  (crate-source "percent-encoding" "2.3.2"
                "083jv1ai930azvawz2khv7w73xh8mnylk7i578cifndjn5y64kwv"))

(define rust-pin-project-1.1.11
  (crate-source "pin-project" "1.1.11"
                "05zm3y3bl83ypsr6favxvny2kys4i19jiz1y18ylrbxwsiz9qx7i"))

(define rust-pin-project-internal-1.1.11
  (crate-source "pin-project-internal" "1.1.11"
                "1ik4mpb92da75inmjvxf2qm61vrnwml3x24wddvrjlqh1z9hxcnr"))

(define rust-pin-project-lite-0.2.17
  (crate-source "pin-project-lite" "0.2.17"
                "1kfmwvs271si96zay4mm8887v5khw0c27jc9srw1a75ykvgj54x8"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

(define rust-plain-0.2.3
  (crate-source "plain" "0.2.3"
                "19n1xbxb4wa7w891268bzf6cbwq4qvdb86bik1z129qb0xnnnndl"))

(define rust-platforms-3.8.0
  (crate-source "platforms" "3.8.0"
                "03i9hp6c484743dzi76i1cp15idvk22dyfg6wzwbxzrnqj1zqim5"))

(define rust-platforms-3.9.0
  (crate-source "platforms" "3.9.0"
                "0ignldrh7x7y8scb2ldlx9lvll5d772jsv2s10nl29z9k5hg4bx2"))

(define rust-portable-atomic-1.13.1
  (crate-source "portable-atomic" "1.13.1"
                "0j8vlar3n5acyigq8q6f4wjx3k3s5yz0rlpqrv76j73gi5qr8fn3"))

(define rust-potential-utf-0.1.4
  (crate-source "potential_utf" "0.1.4"
                "0xxg0pkfpq299wvwln409z4fk80rbv55phh3f1jhjajy5x1ljfdp"))

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

(define rust-ppv-lite86-0.2.21
  (crate-source "ppv-lite86" "0.2.21"
                "1abxx6qz5qnd43br1dd9b2savpihzjza8gb4fbzdql1gxp2f7sl5"))

(define rust-prettyplease-0.2.37
  (crate-source "prettyplease" "0.2.37"
                "0azn11i1kh0byabhsgab6kqs74zyrg69xkirzgqyhz6xmjnsi727"))

(define rust-proc-macro2-1.0.106
  (crate-source "proc-macro2" "1.0.106"
                "0d09nczyaj67x4ihqr5p7gxbkz38gxhk4asc0k8q23g9n85hzl4g"))

(define rust-proptest-1.10.0
  (crate-source "proptest" "1.10.0"
                "0ch5r381al5z7089j47gkyybzbgygkgld5bzfg019vxcznrnqmip"))

(define rust-prost-0.14.3
  (crate-source "prost" "0.14.3"
                "0s057z9nzggzy7x4bbsiar852hg7zb81f4z4phcdb0ig99971snj"))

(define rust-prost-derive-0.14.3
  (crate-source "prost-derive" "0.14.3"
                "02zvva6kb0pfvlyc4nac6gd37ncjrs8jq5scxcq4nbqkc8wh5ii7"))

(define rust-pulldown-cmark-0.13.1
  (crate-source "pulldown-cmark" "0.13.1"
                "19jmxfmcwz5z1hnp74b2nmhx4lbqhvq6ia7kwx6w82prz3xixi43"))

(define rust-quick-error-1.2.3
  (crate-source "quick-error" "1.2.3"
                "1q6za3v78hsspisc197bg3g7rpc989qycy8ypr8ap8igv10ikl51"))

(define rust-quote-1.0.44
  (crate-source "quote" "1.0.44"
                "1r7c7hxl66vz3q9qizgjhy77pdrrypqgk4ghc7260xvvfb7ypci1"))

(define rust-quote-1.0.45
  (crate-source "quote" "1.0.45"
                "095rb5rg7pbnwdp6v8w5jw93wndwyijgci1b5lw8j1h5cscn3wj1"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-r-efi-6.0.0
  (crate-source "r-efi" "6.0.0"
                "1gyrl2k5fyzj9k7kchg2n296z5881lg7070msabid09asp3wkp7q"))

(define rust-rand-0.10.0
  (crate-source "rand" "0.10.0"
                "1y7g1zddjzhzwg0k1nddsfyfaq89a7igpcf7q44mqv6z2frnw9mw"))

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-0.9.2
  (crate-source "rand" "0.9.2"
                "1lah73ainvrgl7brcxx0pwhpnqa3sm3qaj672034jz8i0q7pgckd"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-chacha-0.9.0
  (crate-source "rand_chacha" "0.9.0"
                "1jr5ygix7r60pz0s1cv3ms1f6pd1i9pcdmnxzzhjc3zn3mgjn0nk"))

(define rust-rand-core-0.10.0
  (crate-source "rand_core" "0.10.0"
                "1flazfw1q1hbvadwzmaliplz0xnnjijdnbmzxnzdqplhfzb0z38c"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-rand-core-0.9.5
  (crate-source "rand_core" "0.9.5"
                "0g6qc5r3f0hdmz9b11nripyp9qqrzb0xqk9piip8w8qlvqkcibvn"))

(define rust-rand-xorshift-0.4.0
  (crate-source "rand_xorshift" "0.4.0"
                "0njsn25pis742gb6b89cpq7jp48v9n23a9fvks10yczwks8n4fai"))

(define rust-rayon-1.11.0
  (crate-source "rayon" "1.11.0"
                "13x5fxb7rn4j2yw0cr26n7782jkc7rjzmdkg42qxk3xz0p8033rn"))

(define rust-rayon-core-1.13.0
  (crate-source "rayon-core" "1.13.0"
                "14dbr0sq83a6lf1rfjq5xdpk5r6zgzvmzs5j6110vlv2007qpq92"))

(define rust-redox-syscall-0.7.3
  (crate-source "redox_syscall" "0.7.3"
                "05mys0g4faa5l7dqvl4y8395b42yshs2qlvysdvijlwhx1s0mrvc"))

(define rust-regex-1.12.3
  (crate-source "regex" "1.12.3"
                "0xp2q0x7ybmpa5zlgaz00p8zswcirj9h8nry3rxxsdwi9fhm81z1"))

(define rust-regex-automata-0.1.10
  (crate-source "regex-automata" "0.1.10"
                "0ci1hvbzhrfby5fdpf4ganhf7kla58acad9i1ff1p34dzdrhs8vc"))

(define rust-regex-automata-0.4.14
  (crate-source "regex-automata" "0.4.14"
                "13xf7hhn4qmgfh784llcp2kzrvljd13lb2b1ca0mwnf15w9d87bf"))

(define rust-regex-syntax-0.6.29
  (crate-source "regex-syntax" "0.6.29"
                "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi"))

(define rust-regex-syntax-0.8.10
  (crate-source "regex-syntax" "0.8.10"
                "02jx311ka0daxxc7v45ikzhcl3iydjbbb0mdrpc1xgg8v7c7v2fw"))

(define rust-remove-dir-all-1.0.0
  (crate-source "remove_dir_all" "1.0.0"
                "026xl6wlkjxksm1n3dcccygssami56aa937h6vgnmxxcfnsc1340"))

(define rust-reqwest-0.12.28
  (crate-source "reqwest" "0.12.28"
                "0iqidijghgqbzl3bjg5hb4zmigwa4r612bgi0yiq0c90b6jkrpgd"))

(define rust-reqwest-0.13.2
  (crate-source "reqwest" "0.13.2"
                "00d8xyrbcp0519rr9rhl685ymb6hi3lv0i2bca5lic9s53il6gxb"))

(define rust-retry-2.2.0
  (crate-source "retry" "2.2.0"
                "0kys3dy797wwz782yzakdmldpwqqh1w9zriya876cdy78g9rpaqw"))

(define rust-ring-0.17.14
  (crate-source "ring" "0.17.14"
                "1dw32gv19ccq4hsx3ribhpdzri1vnrlcfqb2vj41xn4l49n9ws54"))

(define rust-rs-tracing-1.1.0
  (crate-source "rs_tracing" "1.1.0"
                "1q31g35vjg9cwg677h2df3wdf38mvg4p4y8f270f29x61mkj3cg3"))

(define rust-rustix-1.1.4
  (crate-source "rustix" "1.1.4"
                "14511f9yjqh0ix07xjrjpllah3325774gfwi9zpq72sip5jlbzmn"))

(define rust-rustls-0.23.37
  (crate-source "rustls" "0.23.37"
                "193k5h0wcih6ghvkrxyzwncivr1bd3a8yw3lzp13pzfcbz5jb03m"))

(define rust-rustls-native-certs-0.8.3
  (crate-source "rustls-native-certs" "0.8.3"
                "0qrajg2n90bcr3bcq6j95gjm7a9lirfkkdmjj32419dyyzan0931"))

(define rust-rustls-pki-types-1.14.0
  (crate-source "rustls-pki-types" "1.14.0"
                "1p9zsgslvwzzkzhm6bqicffqndr4jpx67992b0vl0pi21a5hy15y"))

(define rust-rustls-platform-verifier-0.6.2
  (crate-source "rustls-platform-verifier" "0.6.2"
                "110pqkn3px9115pb6h6a23cq738v29gbp559dfvpmbibqzmzx68x"))

(define rust-rustls-platform-verifier-android-0.1.1
  (crate-source "rustls-platform-verifier-android" "0.1.1"
                "13vq6sxsgz9547xm2zbdxiw8x7ad1g8n8ax6xvxsjqszk7q6awgq"))

(define rust-rustls-webpki-0.103.10
  (crate-source "rustls-webpki" "0.103.10"
                "1vyipcdbazvhl6kyi1m8n0bg98sk25iv12bby2xcly653awb4cyz"))

(define rust-rustls-webpki-0.103.9
  (crate-source "rustls-webpki" "0.103.9"
                "0lwg1nnyv7pp2lfwwjhy81bxm233am99jnsp3iymdhd6k8827pyp"))

(define rust-rustversion-1.0.22
  (crate-source "rustversion" "1.0.22"
                "0vfl70jhv72scd9rfqgr2n11m5i9l1acnk684m2w83w0zbqdx75k"))

(define rust-rusty-fork-0.3.1
  (crate-source "rusty-fork" "0.3.1"
                "1qkf9rvz2irb1wlbkrhrns8n9hnax48z1lgql5nqyr2fyagzfsyc"))

(define rust-ryu-1.0.23
  (crate-source "ryu" "1.0.23"
                "0zs70sg00l2fb9jwrf6cbkdyscjs53anrvai2hf7npyyfi5blx4p"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-schannel-0.1.28
  (crate-source "schannel" "0.1.28"
                "1qb6s5gyxfz2inz753a4z3mc1d266mwvz0c5w7ppd3h44swq27c9"))

(define rust-schannel-0.1.29
  (crate-source "schannel" "0.1.29"
                "0ffrzz5vf2s3gnzvphgb5gg8fqifvryl07qcf7q3x1scj3jbghci"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-security-framework-3.7.0
  (crate-source "security-framework" "3.7.0"
                "07fd0j29j8yczb3hd430vwz784lx9knb5xwbvqna1nbkbivvrx5p"))

(define rust-security-framework-sys-2.17.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "security-framework-sys" "2.17.0"
                "1qr0w0y9iwvmv3hwg653q1igngnc5b74xcf0679cbv23z0fnkqkc"))

(define rust-semver-1.0.27
  (crate-source "semver" "1.0.27"
                "1qmi3akfrnqc2hfkdgcxhld5bv961wbk8my3ascv5068mc5fnryp"))

(define rust-serde-1.0.228
  (crate-source "serde" "1.0.228"
                "17mf4hhjxv5m90g42wmlbc61hdhlm6j9hwfkpcnd72rpgzm993ls"))

(define rust-serde-core-1.0.228
  (crate-source "serde_core" "1.0.228"
                "1bb7id2xwx8izq50098s5j2sqrrvk31jbbrjqygyan6ask3qbls1"))

(define rust-serde-derive-1.0.228
  (crate-source "serde_derive" "1.0.228"
                "0y8xm7fvmr2kjcd029g9fijpndh8csv5m20g4bd76w8qschg4h6m"))

(define rust-serde-json-1.0.149
  (crate-source "serde_json" "1.0.149"
                "11jdx4vilzrjjd1dpgy67x5lgzr0laplz30dhv75lnf5ffa07z43"))

(define rust-serde-spanned-1.0.4
  (crate-source "serde_spanned" "1.0.4"
                "0xkp0qdzams5sqwndbw3xrhf4c0bb5r46w2ywkp1aqsdb8ggkfzq"))

(define rust-serde-urlencoded-0.7.1
  (crate-source "serde_urlencoded" "0.7.1"
                "1zgklbdaysj3230xivihs30qi5vkhigg323a9m62k8jwf4a1qjfk"))

(define rust-sha2-0.10.9
  (crate-source "sha2" "0.10.9"
                "10xjj843v31ghsksd9sl9y12qfc48157j1xpb8v1ml39jy0psl57"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-simd-adler32-0.3.8
  (crate-source "simd-adler32" "0.3.8"
                "18lx2gdgislabbvlgw5q3j5ssrr77v8kmkrxaanp3liimp2sc873"))

(define rust-similar-2.7.0
  (crate-source "similar" "2.7.0"
                "1aidids7ymfr96s70232s6962v5g9l4zwhkvcjp4c5hlb6b5vfxv"))

(define rust-slab-0.4.12
  (crate-source "slab" "0.4.12"
                "1xcwik6s6zbd3lf51kkrcicdq2j4c1fw0yjdai2apy9467i0sy8c"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-snapbox-1.0.0
  (crate-source "snapbox" "1.0.0"
                "1sl9vxjkbxngy7s5b3wg09ssybdxqjzpmxq8hzlcnm40nrqhmmvi"))

(define rust-snapbox-1.1.0
  (crate-source "snapbox" "1.1.0"
                "13dzmhph5zmcx7kfdb752c0lbz772x5mxlcbjc6wbxwwlwyy98mp"))

(define rust-snapbox-macros-1.0.0
  (crate-source "snapbox-macros" "1.0.0"
                "1ygxy7mxypgigfajmbd9ks27s6rmhmlkgh29f4psnmhl5vscwj6j"))

(define rust-snapbox-macros-1.0.1
  (crate-source "snapbox-macros" "1.0.1"
                "11jk2l92k5cdcakxcqc5wwpgf7iiyrpk6xk602k0jhs07m24hann"))

(define rust-socket2-0.6.2
  (crate-source "socket2" "0.6.2"
                "1q073zkvz96h216mfz6niqk2kjqrgqv2va6zj34qh84zv4xamx46"))

(define rust-socket2-0.6.3
  (crate-source "socket2" "0.6.3"
                "0gkjjcyn69hqhhlh5kl8byk5m0d7hyrp2aqwzbs3d33q208nwxis"))

(define rust-stable-deref-trait-1.2.1
  (crate-source "stable_deref_trait" "1.2.1"
                "15h5h73ppqyhdhx6ywxfj88azmrpml9gl6zp3pwy2malqa6vxqkc"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-subtle-2.6.1
  (crate-source "subtle" "2.6.1"
                "14ijxaymghbl1p0wql9cib5zlwiina7kall6w7g89csprkgbvhhk"))

(define rust-syn-2.0.117
  (crate-source "syn" "2.0.117"
                "16cv7c0wbn8amxc54n4w15kxlx5ypdmla8s0gxr2l7bv7s0bhrg6"))

(define rust-sync-wrapper-1.0.2
  (crate-source "sync_wrapper" "1.0.2"
                "0qvjyasd6w18mjg5xlaq5jgy84jsjfsvmnn12c13gypxbv75dwhb"))

(define rust-synstructure-0.13.2
  (crate-source "synstructure" "0.13.2"
                "1lh9lx3r3jb18f8sbj29am5hm9jymvbwh6jb1izsnnxgvgrp12kj"))

(define rust-sys-info-0.9.1
  (crate-source "sys-info" "0.9.1"
                "0b759814ng0cj5a1iiqqjgrzfg9vqlpkbp6z3l76mycbp850sfhb"))

(define rust-tar-0.4.44
  (crate-source "tar" "0.4.44"
                "0yk69a8j9xv51mdcy0853jai5zh1pd9yn456q4cpmj0js9w3i1hx"))

(define rust-tar-0.4.45
  (crate-source "tar" "0.4.45"
                "0wq90hif25348zrvmk88q01g8aj8v8pla7f1vxgsf7x2frj2ls92"))

(define rust-tempfile-3.26.0
  (crate-source "tempfile" "3.26.0"
                "182lfcv9d5w9349i0rjlgn4431k2m3yqfn9ls84p9d3ifxv2r9w2"))

(define rust-tempfile-3.27.0
  (crate-source "tempfile" "3.27.0"
                "1gblhnyfjsbg9wjg194n89wrzah7jy3yzgnyzhp56f3v9jd7wj9j"))

(define rust-terminal-size-0.4.3
  (crate-source "terminal_size" "0.4.3"
                "1l7cicmz49c0cyskfp5a389rsai649xi7y032v73475ikjbwpf30"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.18
  (crate-source "thiserror" "2.0.18"
                "1i7vcmw9900bvsmay7mww04ahahab7wmr8s925xc083rpjybb222"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.18
  (crate-source "thiserror-impl" "2.0.18"
                "1mf1vrbbimj1g6dvhdgzjmn6q09yflz2b92zs1j9n3k7cxzyxi7b"))

(define rust-thread-local-1.1.9
  (crate-source "thread_local" "1.1.9"
                "1191jvl8d63agnq06pcnarivf63qzgpws5xa33hgc92gjjj4c0pn"))

(define rust-threadpool-1.8.1
  (crate-source "threadpool" "1.8.1"
                "1amgfyzvynbm8pacniivzq9r0fh3chhs7kijic81j76l6c5ycl6h"))

(define rust-time-0.3.47
  (crate-source "time" "0.3.47"
                "0b7g9ly2iabrlgizliz6v5x23yq5d6bpp0mqz6407z1s526d8fvl"))

(define rust-time-core-0.1.8
  (crate-source "time-core" "0.1.8"
                "1jidl426mw48i7hjj4hs9vxgd9lwqq4vyalm4q8d7y4iwz7y353n"))

(define rust-time-macros-0.2.27
  (crate-source "time-macros" "0.2.27"
                "058ja265waq275wxvnfwavbz9r1hd4dgwpfn7a1a9a70l32y8w1f"))

(define rust-tinystr-0.8.2
  (crate-source "tinystr" "0.8.2"
                "0sa8z88axdsf088hgw5p4xcyi6g3w3sgbb6qdp81bph9bk2fkls2"))

(define rust-tokio-1.49.0
  (crate-source "tokio" "1.49.0"
                "11ix3pl03s0bp71q3wddrbf8xr0cpn47d7fzr6m42r3kswy918kj"))

(define rust-tokio-1.50.0
  (crate-source "tokio" "1.50.0"
                "0bc2c5kd57p2xd4l6hagb0bkrp798k5vw0f3xzzwy0sf6ws5xb97"))

(define rust-tokio-macros-2.6.0
  (crate-source "tokio-macros" "2.6.0"
                "19czvgliginbzyhhfbmj77wazqn2y8g27y2nirfajdlm41bphh5g"))

(define rust-tokio-macros-2.6.1
  (crate-source "tokio-macros" "2.6.1"
                "172nwz3s7mmh266hb8l5xdnc7v9kqahisppqhinfd75nz3ps4maw"))

(define rust-tokio-native-tls-0.3.1
  (crate-source "tokio-native-tls" "0.3.1"
                "1wkfg6zn85zckmv4im7mv20ca6b1vmlib5xwz9p7g19wjfmpdbmv"))

(define rust-tokio-retry-0.3.0
  (crate-source "tokio-retry" "0.3.0"
                "0kr1hnm5dmb9gfkby88yg2xj8g6x4i4gipva0c8ca3xyxhvfnmvz"))

(define rust-tokio-rustls-0.26.4
  (crate-source "tokio-rustls" "0.26.4"
                "0qggwknz9w4bbsv1z158hlnpkm97j3w8v31586jipn99byaala8p"))

(define rust-tokio-stream-0.1.18
  (crate-source "tokio-stream" "0.1.18"
                "0w3cj33605ab58wqd382gnla5pnd9hnr00xgg333np5bka04knij"))

(define rust-tokio-util-0.7.18
  (crate-source "tokio-util" "0.7.18"
                "1600rd47pylwn7cap1k7s5nvdaa9j7w8kqigzp1qy7mh0p4cxscs"))

(define rust-toml-1.0.3+spec-1.1.0
  (crate-source "toml" "1.0.3+spec-1.1.0"
                "033cc36cl3w7mfq9xzgxnslz573j06idzb94vd3q70dd36plwqf7"))

(define rust-toml-1.0.7+spec-1.1.0
  (crate-source "toml" "1.0.7+spec-1.1.0"
                "15kaclc4y8yb4ahny19ng51rmff4vj7lyy5qq25lavkgi9yxaa6x"))

(define rust-toml-datetime-1.0.0+spec-1.1.0
  (crate-source "toml_datetime" "1.0.0+spec-1.1.0"
                "0gpiaddhignli6whj52ysjxwmmy82r8qxihckzss8y4md5f5bhij"))

(define rust-toml-datetime-1.0.1+spec-1.1.0
  (crate-source "toml_datetime" "1.0.1+spec-1.1.0"
                "1sgk7zc6x187iib7kj1nzn44mp0zrk9hgii69rbar35m3ms0wclv"))

(define rust-toml-parser-1.0.10+spec-1.1.0
  (crate-source "toml_parser" "1.0.10+spec-1.1.0"
                "081lsv63zphnff9ssb0yjavcc82sblvj808rvwb4h76kxx5mpwkx"))

(define rust-toml-parser-1.0.9+spec-1.1.0
  (crate-source "toml_parser" "1.0.9+spec-1.1.0"
                "1i54qpvvcppy8ybdn9gssas81vfzq0kmgkcnxzhyf8w9w0al8bbh"))

(define rust-toml-writer-1.0.6+spec-1.1.0
  (crate-source "toml_writer" "1.0.6+spec-1.1.0"
                "01r6x42d1p8p5kzfsi1fm4dakm3w53vi69f2ivyqpvi1xm5g25mb"))

(define rust-toml-writer-1.0.7+spec-1.1.0
  (crate-source "toml_writer" "1.0.7+spec-1.1.0"
                "0vdmlskpqkjf5n2zghna8mwlqdbf0ryskfxnlhfjphixdqfalypi"))

(define rust-tonic-0.14.5
  (crate-source "tonic" "0.14.5"
                "1v4k7aa28m7722gz9qak2jiy7lis1ycm4fdmq63iip4m0qdcdizy"))

(define rust-tonic-prost-0.14.5
  (crate-source "tonic-prost" "0.14.5"
                "02fkg2bv87q0yds2wz3w0s7i1x6qcgbrl00dy6ipajdapfh7clx5"))

(define rust-tower-0.5.3
  (crate-source "tower" "0.5.3"
                "1m5i3a2z1sgs8nnz1hgfq2nr4clpdmizlp1d9qsg358ma5iyzrgb"))

(define rust-tower-http-0.6.8
  (crate-source "tower-http" "0.6.8"
                "1y514jwzbyrmrkbaajpwmss4rg0mak82k16d6588w9ncaffmbrnl"))

(define rust-tower-layer-0.3.3
  (crate-source "tower-layer" "0.3.3"
                "03kq92fdzxin51w8iqix06dcfgydyvx7yr6izjq0p626v9n2l70j"))

(define rust-tower-service-0.3.3
  (crate-source "tower-service" "0.3.3"
                "1hzfkvkci33ra94xjx64vv3pp0sq346w06fpkcdwjcid7zhvdycd"))

(define rust-tracing-0.1.44
  (crate-source "tracing" "0.1.44"
                "006ilqkg1lmfdh3xhg3z762izfwmxcvz0w7m4qx2qajbz9i1drv3"))

(define rust-tracing-attributes-0.1.31
  (crate-source "tracing-attributes" "0.1.31"
                "1np8d77shfvz0n7camx2bsf1qw0zg331lra0hxb4cdwnxjjwz43l"))

(define rust-tracing-core-0.1.36
  (crate-source "tracing-core" "0.1.36"
                "16mpbz6p8vd6j7sf925k9k8wzvm9vdfsjbynbmaxxyq6v7wwm5yv"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-opentelemetry-0.32.0
  (crate-source "tracing-opentelemetry" "0.32.0"
                "13hvpfljbxi8id3j5pmzn4rhc4nkw68pfp57mf4q1n1x8rc5cvhy"))

(define rust-tracing-opentelemetry-0.32.1
  (crate-source "tracing-opentelemetry" "0.32.1"
                "1z2jjmxbkm1qawlb3bm99x8xwf4g8wjkbcknm9z4fv1w14nqzhhs"))

(define rust-tracing-subscriber-0.3.19
  (crate-source "tracing-subscriber" "0.3.19"
                "0220rignck8072i89jjsh140vmh14ydwpdwnifyaf3xcnpn9s678"))

(define rust-try-lock-0.2.5
  (crate-source "try-lock" "0.2.5"
                "0jqijrrvm1pyq34zn1jmy2vihd4jcrjlvsh4alkjahhssjnsn8g4"))

(define rust-typenum-1.19.0
  (crate-source "typenum" "1.19.0"
                "1fw2mpbn2vmqan56j1b3fbpcdg80mz26fm53fs16bq5xcq84hban"))

(define rust-unarray-0.1.4
  (crate-source "unarray" "0.1.4"
                "154smf048k84prsdgh09nkm2n0w0336v84jd4zikyn6v6jrqbspa"))

(define rust-unicase-2.9.0
  (crate-source "unicase" "2.9.0"
                "0hh1wrfd7807mfph2q67jsxqgw8hm82xg2fb8ln8cvblkwxbri6v"))

(define rust-unicode-ident-1.0.24
  (crate-source "unicode-ident" "1.0.24"
                "0xfs8y1g7syl2iykji8zk5hgfi5jw819f5zsrbaxmlzwsly33r76"))

(define rust-unicode-width-0.2.2
  (crate-source "unicode-width" "0.2.2"
                "0m7jjzlcccw716dy9423xxh0clys8pfpllc5smvfxrzdf66h9b5l"))

(define rust-unicode-xid-0.2.6
  (crate-source "unicode-xid" "0.2.6"
                "0lzqaky89fq0bcrh6jj6bhlz37scfd8c7dsj5dq7y32if56c1hgb"))

(define rust-unit-prefix-0.5.2
  (crate-source "unit-prefix" "0.5.2"
                "18xr6yhdvlxrv51y6js9npa3qhkzc5b1z4skr5kfzn7kkd449rc1"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"))

(define rust-url-2.5.8
  (crate-source "url" "2.5.8"
                "1v8f7nx3hpr1qh76if0a04sj08k86amsq4h8cvpw6wvk76jahrzz"))

(define rust-utf8-iter-1.0.4
  (crate-source "utf8_iter" "1.0.4"
                "1gmna9flnj8dbyd8ba17zigrp9c4c3zclngf5lnb5yvz1ri41hdn"))

(define rust-utf8-width-0.1.8
  (crate-source "utf8-width" "0.1.8"
                "14d08vrz878wqpmqw46yl5l1vwmdf00zx4i49z8iahdmf3cw14hj"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-vcpkg-0.2.15
  (crate-source "vcpkg" "0.2.15"
                "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-wait-timeout-0.2.1
  (crate-source "wait-timeout" "0.2.1"
                "04azqv9mnfxgvnc8j2wp362xraybakh2dy1nj22gj51rdl93pb09"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))

(define rust-want-0.3.1
  (crate-source "want" "0.3.1"
                "03hbfrnvqqdchb5kgxyavb9jabwza0dmh2vw5kg0dq8rxl57d9xz"))

(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))

(define rust-wasip2-1.0.2+wasi-0.2.9
  (crate-source "wasip2" "1.0.2+wasi-0.2.9"
                "1xdw7v08jpfjdg94sp4lbdgzwa587m5ifpz6fpdnkh02kwizj5wm"))

(define rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
  (crate-source "wasip3" "0.4.0+wasi-0.3.0-rc-2026-01-06"
                "19dc8p0y2mfrvgk3qw3c3240nfbylv22mvyxz84dqpgai2zzha2l"))

(define rust-wasm-bindgen-0.2.114
  (crate-source "wasm-bindgen" "0.2.114"
                "13nkhw552hpllrrmkd2x9y4bmcxr82kdpky2n667kqzcq6jzjck5"))

(define rust-wasm-bindgen-futures-0.4.64
  (crate-source "wasm-bindgen-futures" "0.4.64"
                "1f3xnr40wwims4zhvh119dhwmffz4h4x82cffi118ri878mm5ig9"))

(define rust-wasm-bindgen-macro-0.2.114
  (crate-source "wasm-bindgen-macro" "0.2.114"
                "1rhq9kkl7n0zjrag9p25xsi4aabpgfkyf02zn4xv6pqhrw7xb8hq"))

(define rust-wasm-bindgen-macro-support-0.2.114
  (crate-source "wasm-bindgen-macro-support" "0.2.114"
                "1qriqqjpn922kv5c7f7627fj823k5aifv06j2gvwsiy5map4rkh3"))

(define rust-wasm-bindgen-shared-0.2.114
  (crate-source "wasm-bindgen-shared" "0.2.114"
                "05lc6w64jxlk4wk8rjci4z61lhx2ams90la27a41gvi3qaw2d8vm"))

(define rust-wasm-encoder-0.244.0
  (crate-source "wasm-encoder" "0.244.0"
                "06c35kv4h42vk3k51xjz1x6hn3mqwfswycmr6ziky033zvr6a04r"))

(define rust-wasm-metadata-0.244.0
  (crate-source "wasm-metadata" "0.244.0"
                "02f9dhlnryd2l7zf03whlxai5sv26x4spfibjdvc3g9gd8z3a3mv"))

(define rust-wasm-streams-0.5.0
  (crate-source "wasm-streams" "0.5.0"
                "1fqbcx33w8ys5i5dv3p28a82g4yiclmhn80fcfp137kwa7vc87lx"))

(define rust-wasmparser-0.244.0
  (crate-source "wasmparser" "0.244.0"
                "1zi821hrlsxfhn39nqpmgzc0wk7ax3dv6vrs5cw6kb0v5v3hgf27"))

(define rust-web-sys-0.3.91
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "web-sys" "0.3.91"
                "1y91r8f4dy4iqgrr03swdzqffz6wmllrgninp8kgpaq4n5xs2jw5"))

(define rust-web-time-1.1.0
  (crate-source "web-time" "1.1.0"
                "1fx05yqx83dhx628wb70fyy10yjfq1jpl20qfqhdkymi13rq0ras"))

(define rust-webpki-root-certs-1.0.6
  (crate-source "webpki-root-certs" "1.0.6"
                "1jm844z3caldlsb4ycb2h7q6vw4awfdgmddmx2sgyxi6mjj1hkw0"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-util-0.1.11
  (crate-source "winapi-util" "0.1.11"
                "08hdl7mkll7pz8whg869h58c1r9y7in0w0pk8fm24qc77k0b39y2"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-windows-aarch64-gnullvm-0.42.2
  (crate-source "windows_aarch64_gnullvm" "0.42.2"
                "1y4q0qmvl0lvp7syxvfykafvmwal5hrjb4fmv04bqs0bawc52yjr"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-gnullvm-0.53.1
  (crate-source "windows_aarch64_gnullvm" "0.53.1"
                "0lqvdm510mka9w26vmga7hbkmrw9glzc90l4gya5qbxlm1pl3n59"))

(define rust-windows-aarch64-msvc-0.42.2
  (crate-source "windows_aarch64_msvc" "0.42.2"
                "0hsdikjl5sa1fva5qskpwlxzpc5q9l909fpl1w6yy1hglrj8i3p0"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-aarch64-msvc-0.53.1
  (crate-source "windows_aarch64_msvc" "0.53.1"
                "01jh2adlwx043rji888b22whx4bm8alrk3khjpik5xn20kl85mxr"))

(define rust-windows-i686-gnu-0.42.2
  (crate-source "windows_i686_gnu" "0.42.2"
                "0kx866dfrby88lqs9v1vgmrkk1z6af9lhaghh5maj7d4imyr47f6"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnu-0.53.1
  (crate-source "windows_i686_gnu" "0.53.1"
                "18wkcm82ldyg4figcsidzwbg1pqd49jpm98crfz0j7nqd6h6s3ln"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-gnullvm-0.53.1
  (crate-source "windows_i686_gnullvm" "0.53.1"
                "030qaxqc4salz6l4immfb6sykc6gmhyir9wzn2w8mxj8038mjwzs"))

(define rust-windows-i686-msvc-0.42.2
  (crate-source "windows_i686_msvc" "0.42.2"
                "0q0h9m2aq1pygc199pa5jgc952qhcnf0zn688454i7v4xjv41n24"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

(define rust-windows-i686-msvc-0.53.1
  (crate-source "windows_i686_msvc" "0.53.1"
                "1hi6scw3mn2pbdl30ji5i4y8vvspb9b66l98kkz350pig58wfyhy"))

(define rust-windows-link-0.2.1
  (crate-source "windows-link" "0.2.1"
                "1rag186yfr3xx7piv5rg8b6im2dwcf8zldiflvb22xbzwli5507h"))

(define rust-windows-registry-0.6.1
  (crate-source "windows-registry" "0.6.1"
                "082p7l615qk8a4g8g15yipc5lghga6cgfhm74wm7zknwzgvjnx82"))

(define rust-windows-result-0.4.1
  (crate-source "windows-result" "0.4.1"
                "1d9yhmrmmfqh56zlj751s5wfm9a2aa7az9rd7nn5027nxa4zm0bp"))

(define rust-windows-strings-0.5.1
  (crate-source "windows-strings" "0.5.1"
                "14bhng9jqv4fyl7lqjz3az7vzh8pw0w4am49fsqgcz67d67x0dvq"))

(define rust-windows-sys-0.45.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.45.0"
                "1l36bcqm4g89pknfp8r9rl1w4bn017q6a8qlx8viv0xjxzjkna3m"))

(define rust-windows-sys-0.52.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-sys-0.60.2
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.60.2"
                "1jrbc615ihqnhjhxplr2kw7rasrskv9wj3lr80hgfd42sbj01xgj"))

(define rust-windows-sys-0.61.2
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.61.2"
                "1z7k3y9b6b5h52kid57lvmvm05362zv1v8w0gc7xyv5xphlp44xf"))

(define rust-windows-targets-0.42.2
  (crate-source "windows-targets" "0.42.2"
                "0wfhnib2fisxlx8c507dbmh97kgij4r6kcxdi0f9nk6l1k080lcf"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-targets-0.53.5
  (crate-source "windows-targets" "0.53.5"
                "1wv9j2gv3l6wj3gkw5j1kr6ymb5q6dfc42yvydjhv3mqa7szjia9"))

(define rust-windows-x86-64-gnu-0.42.2
  (crate-source "windows_x86_64_gnu" "0.42.2"
                "0dnbf2xnp3xrvy8v9mgs3var4zq9v9yh9kv79035rdgyp2w15scd"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnu-0.53.1
  (crate-source "windows_x86_64_gnu" "0.53.1"
                "16d4yiysmfdlsrghndr97y57gh3kljkwhfdbcs05m1jasz6l4f4w"))

(define rust-windows-x86-64-gnullvm-0.42.2
  (crate-source "windows_x86_64_gnullvm" "0.42.2"
                "18wl9r8qbsl475j39zvawlidp1bsbinliwfymr43fibdld31pm16"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-gnullvm-0.53.1
  (crate-source "windows_x86_64_gnullvm" "0.53.1"
                "1qbspgv4g3q0vygkg8rnql5c6z3caqv38japiynyivh75ng1gyhg"))

(define rust-windows-x86-64-msvc-0.42.2
  (crate-source "windows_x86_64_msvc" "0.42.2"
                "1w5r0q0yzx827d10dpjza2ww0j8iajqhmb54s735hhaj66imvv4s"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-windows-x86-64-msvc-0.53.1
  (crate-source "windows_x86_64_msvc" "0.53.1"
                "0l6npq76vlq4ksn4bwsncpr8508mk0gmznm6wnhjg95d19gzzfyn"))

(define rust-winnow-0.7.14
  (crate-source "winnow" "0.7.14"
                "0a88ahjqhyn2ln1yplq2xsigm09kxqkdkkk2c2mfxkbzszln8lss"))

(define rust-winnow-1.0.0
  (crate-source "winnow" "1.0.0"
                "1n67gx8mg2b6r2z54zwbrb6qsfbdsar1lvafsfaajr3jcvj8h3m9"))

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

(define rust-xattr-1.6.1
  (crate-source "xattr" "1.6.1"
                "0ml1mb43gqasawillql6b344m0zgq8mz0isi11wj8vbg43a5mr1j"))

(define rust-xz2-0.1.7
  (crate-source "xz2" "0.1.7"
                "1qk7nzpblizvayyq4xzi4b0zacmmbqr6vb9fc0v1avyp17f4931q"))

(define rust-yoke-0.8.1
  (crate-source "yoke" "0.8.1"
                "0m29dm0bf5iakxgma0bj6dbmc3b8qi9b1vaw9sa76kdqmz3fbmkj"))

(define rust-yoke-derive-0.8.1
  (crate-source "yoke-derive" "0.8.1"
                "0pbyja133jnng4mrhimzdq4a0y26421g734ybgz8wsgbfhl0andn"))

(define rust-zerocopy-0.8.40
  (crate-source "zerocopy" "0.8.40"
                "1r9j2mlb54q1l9pgall3mk0gg6cprhdncvbbgsgxnxmmj3jcd2d7"))

(define rust-zerocopy-0.8.47
  (crate-source "zerocopy" "0.8.47"
                "11zdl3708210fsiax93qbvw8kiadg9lnzriw26xg44g35c32mfzg"))

(define rust-zerocopy-derive-0.8.40
  (crate-source "zerocopy-derive" "0.8.40"
                "0lsrhg5nvf0c40z644a014l2nrvh7xw0ff3i9744k9vif2d4hp7n"))

(define rust-zerocopy-derive-0.8.47
  (crate-source "zerocopy-derive" "0.8.47"
                "12dbrk2w8mszdq9v01ls930bi446iyk4llggxrx8whalkckcg2qf"))

(define rust-zerofrom-0.1.6
  (crate-source "zerofrom" "0.1.6"
                "19dyky67zkjichsb7ykhv0aqws3q0jfvzww76l66c19y6gh45k2h"))

(define rust-zerofrom-derive-0.1.6
  (crate-source "zerofrom-derive" "0.1.6"
                "00l5niw7c1b0lf1vhvajpjmcnbdp2vn96jg4nmkhq2db0rp5s7np"))

(define rust-zeroize-1.8.2
  (crate-source "zeroize" "1.8.2"
                "1l48zxgcv34d7kjskr610zqsm6j2b4fcr2vfh9jm9j1jgvk58wdr"))

(define rust-zerotrie-0.2.3
  (crate-source "zerotrie" "0.2.3"
                "0lbqznlqazmrwwzslw0ci7p3pqxykrbfhq29npj0gmb2amxc2n9a"))

(define rust-zerovec-0.11.5
  (crate-source "zerovec" "0.11.5"
                "00m0p47k2g9mkv505hky5xh3r6ps7v8qc0dy4pspg542jj972a3c"))

(define rust-zerovec-derive-0.11.2
  (crate-source "zerovec-derive" "0.11.2"
                "1wsig4h5j7a1scd5hrlnragnazjny9qjc44hancb6p6a76ay7p7a"))

(define rust-zlib-rs-0.6.2
  (crate-source "zlib-rs" "0.6.2"
                "1j1nf9nmmx9wcwf0mn22smpam7wb28sdz6fw6v8pwcq7227c8if7"))

(define rust-zlib-rs-0.6.3
  (crate-source "zlib-rs" "0.6.3"
                "04qmv85amq6sv73bzqgvnlsk9mnrl97rygzf2v4zjcx1807d9qrv"))

(define rust-zmij-1.0.21
  (crate-source "zmij" "1.0.21"
                "1amb5i6gz7yjb0dnmz5y669674pqmwbj44p4yfxfv2ncgvk8x15q"))

(define rust-zstd-0.13.3
  (crate-source "zstd" "0.13.3"
                "12n0h4w9l526li7jl972rxpyf012jw3nwmji2qbjghv9ll8y67p9"))

(define rust-zstd-safe-7.2.4
  (crate-source "zstd-safe" "7.2.4"
                "179vxmkzhpz6cq6mfzvgwc99bpgllkr6lwxq7ylh5dmby3aw8jcg"))

(define rust-zstd-sys-2.0.16+zstd.1.5.7
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "zstd-sys" "2.0.16+zstd.1.5.7"
                "0j1pd2iaqpvaxlgqmmijj68wma7xwdv9grrr63j873yw5ay9xqci"))

(define-cargo-inputs lookup-cargo-inputs
                     (rustup =>
                             (list rust-adler2-2.0.1
                              rust-aho-corasick-1.1.4
                              rust-aligned-0.4.3
                              rust-anstream-0.6.21
                              rust-anstream-1.0.0
                              rust-anstyle-1.0.13
                              rust-anstyle-lossy-1.1.4
                              rust-anstyle-parse-0.2.7
                              rust-anstyle-parse-1.0.0
                              rust-anstyle-query-1.1.5
                              rust-anstyle-svg-0.1.12
                              rust-anstyle-wincon-3.0.11
                              rust-anyhow-1.0.102
                              rust-as-slice-0.2.1
                              rust-async-compression-0.4.41
                              rust-async-trait-0.1.89
                              rust-atomic-waker-1.1.2
                              rust-autocfg-1.5.0
                              rust-aws-lc-rs-1.16.1
                              rust-aws-lc-sys-0.38.0
                              rust-base64-0.22.1
                              rust-bit-set-0.8.0
                              rust-bit-vec-0.8.0
                              rust-bitflags-2.11.0
                              rust-block-buffer-0.10.4
                              rust-bstr-1.12.1
                              rust-bumpalo-3.20.2
                              rust-bytes-1.11.1
                              rust-cc-1.2.56
                              rust-cesu8-1.1.0
                              rust-cfg-if-0.1.10
                              rust-cfg-if-1.0.4
                              rust-cfg-aliases-0.2.1
                              rust-chacha20-0.10.0
                              rust-chrono-0.4.44
                              rust-clap-4.5.60
                              rust-clap-cargo-0.18.3
                              rust-clap-builder-4.5.60
                              rust-clap-complete-4.5.66
                              rust-clap-derive-4.5.55
                              rust-clap-lex-1.0.0
                              rust-cmake-0.1.57
                              rust-colorchoice-1.0.4
                              rust-combine-4.6.7
                              rust-compression-codecs-0.4.37
                              rust-compression-core-0.4.31
                              rust-console-0.16.2
                              rust-core-foundation-0.10.1
                              rust-core-foundation-sys-0.8.7
                              rust-cpufeatures-0.2.17
                              rust-cpufeatures-0.3.0
                              rust-crc32fast-1.5.0
                              rust-crossbeam-deque-0.8.6
                              rust-crossbeam-epoch-0.9.18
                              rust-crossbeam-utils-0.8.21
                              rust-crypto-common-0.1.7
                              rust-curl-0.4.49
                              rust-curl-sys-0.4.85+curl-8.18.0
                              rust-cvt-0.1.2
                              rust-deranged-0.5.8
                              rust-digest-0.10.7
                              rust-displaydoc-0.2.5
                              rust-dunce-1.0.5
                              rust-effective-limits-0.5.5
                              rust-either-1.15.0
                              rust-encode-unicode-1.0.0
                              rust-enum-map-2.7.3
                              rust-enum-map-derive-0.17.0
                              rust-env-proxy-0.4.1
                              rust-equivalent-1.0.2
                              rust-errno-0.3.14
                              rust-fastrand-2.3.0
                              rust-filetime-0.2.27
                              rust-find-msvc-tools-0.1.9
                              rust-flate2-1.1.9
                              rust-fnv-1.0.7
                              rust-foldhash-0.1.5
                              rust-foreign-types-0.3.2
                              rust-foreign-types-shared-0.1.1
                              rust-form-urlencoded-1.2.2
                              rust-fs-at-0.2.1
                              rust-fs-extra-1.3.0
                              rust-futures-channel-0.3.32
                              rust-futures-core-0.3.32
                              rust-futures-executor-0.3.32
                              rust-futures-io-0.3.32
                              rust-futures-macro-0.3.32
                              rust-futures-sink-0.3.32
                              rust-futures-task-0.3.32
                              rust-futures-util-0.3.32
                              rust-generic-array-0.14.7
                              rust-getrandom-0.2.17
                              rust-getrandom-0.3.4
                              rust-getrandom-0.4.1
                              rust-git-testament-0.2.6
                              rust-git-testament-derive-0.2.1
                              rust-h2-0.4.13
                              rust-hashbrown-0.15.5
                              rust-hashbrown-0.16.1
                              rust-heck-0.5.0
                              rust-hermit-abi-0.5.2
                              rust-home-0.5.12
                              rust-html-escape-0.2.13
                              rust-http-1.4.0
                              rust-http-body-1.0.1
                              rust-http-body-util-0.1.3
                              rust-httparse-1.10.1
                              rust-httpdate-1.0.3
                              rust-hyper-1.8.1
                              rust-hyper-rustls-0.27.7
                              rust-hyper-timeout-0.5.2
                              rust-hyper-tls-0.6.0
                              rust-hyper-util-0.1.20
                              rust-icu-collections-2.1.1
                              rust-icu-locale-core-2.1.1
                              rust-icu-normalizer-2.1.1
                              rust-icu-normalizer-data-2.1.1
                              rust-icu-properties-2.1.2
                              rust-icu-properties-data-2.1.2
                              rust-icu-provider-2.1.1
                              rust-id-arena-2.3.0
                              rust-idna-1.1.0
                              rust-idna-adapter-1.2.1
                              rust-indexmap-2.13.0
                              rust-indicatif-0.18.4
                              rust-ipnet-2.11.0
                              rust-iri-string-0.7.10
                              rust-is-terminal-polyfill-1.70.2
                              rust-itertools-0.14.0
                              rust-itoa-1.0.17
                              rust-jni-0.21.1
                              rust-jni-sys-0.3.0
                              rust-jobserver-0.1.34
                              rust-js-sys-0.3.91
                              rust-lazy-static-1.5.0
                              rust-leb128fmt-0.1.0
                              rust-libc-0.2.182
                              rust-libredox-0.1.14
                              rust-libz-sys-1.1.24
                              rust-linux-raw-sys-0.12.1
                              rust-litemap-0.8.1
                              rust-log-0.4.29
                              rust-lzma-sys-0.1.20
                              rust-matchers-0.1.0
                              rust-memchr-2.8.0
                              rust-miniz-oxide-0.8.9
                              rust-mio-1.1.1
                              rust-native-tls-0.2.18
                              rust-nix-0.29.0
                              rust-normalize-line-endings-0.3.0
                              rust-normpath-1.5.0
                              rust-nu-ansi-term-0.46.0
                              rust-num-conv-0.2.0
                              rust-num-traits-0.2.19
                              rust-num-cpus-1.17.0
                              rust-once-cell-1.21.3
                              rust-once-cell-polyfill-1.70.2
                              rust-opener-0.8.4
                              rust-openssl-0.10.75
                              rust-openssl-macros-0.1.1
                              rust-openssl-probe-0.1.6
                              rust-openssl-probe-0.2.1
                              rust-openssl-src-300.5.4+3.5.4
                              rust-openssl-sys-0.9.111
                              rust-opentelemetry-0.31.0
                              rust-opentelemetry-http-0.31.0
                              rust-opentelemetry-otlp-0.31.0
                              rust-opentelemetry-proto-0.31.0
                              rust-opentelemetry-sdk-0.31.0
                              rust-overload-0.1.1
                              rust-percent-encoding-2.3.2
                              rust-pin-project-1.1.11
                              rust-pin-project-internal-1.1.11
                              rust-pin-project-lite-0.2.17
                              rust-pin-utils-0.1.0
                              rust-pkg-config-0.3.32
                              rust-plain-0.2.3
                              rust-platforms-3.8.0
                              rust-portable-atomic-1.13.1
                              rust-potential-utf-0.1.4
                              rust-powerfmt-0.2.0
                              rust-ppv-lite86-0.2.21
                              rust-prettyplease-0.2.37
                              rust-proc-macro2-1.0.106
                              rust-proptest-1.10.0
                              rust-prost-0.14.3
                              rust-prost-derive-0.14.3
                              rust-pulldown-cmark-0.13.1
                              rust-quick-error-1.2.3
                              rust-quote-1.0.44
                              rust-r-efi-5.3.0
                              rust-rand-0.8.5
                              rust-rand-0.9.2
                              rust-rand-0.10.0
                              rust-rand-chacha-0.3.1
                              rust-rand-chacha-0.9.0
                              rust-rand-core-0.6.4
                              rust-rand-core-0.9.5
                              rust-rand-core-0.10.0
                              rust-rand-xorshift-0.4.0
                              rust-rayon-1.11.0
                              rust-rayon-core-1.13.0
                              rust-redox-syscall-0.7.3
                              rust-regex-1.12.3
                              rust-regex-automata-0.1.10
                              rust-regex-automata-0.4.14
                              rust-regex-syntax-0.6.29
                              rust-regex-syntax-0.8.10
                              rust-remove-dir-all-1.0.0
                              rust-reqwest-0.12.28
                              rust-reqwest-0.13.2
                              rust-retry-2.2.0
                              rust-ring-0.17.14
                              rust-rs-tracing-1.1.0
                              rust-rustix-1.1.4
                              rust-rustls-0.23.37
                              rust-rustls-native-certs-0.8.3
                              rust-rustls-pki-types-1.14.0
                              rust-rustls-platform-verifier-0.6.2
                              rust-rustls-platform-verifier-android-0.1.1
                              rust-rustls-webpki-0.103.9
                              rust-rustversion-1.0.22
                              rust-rusty-fork-0.3.1
                              rust-ryu-1.0.23
                              rust-same-file-1.0.6
                              rust-schannel-0.1.28
                              rust-scopeguard-1.2.0
                              rust-security-framework-3.7.0
                              rust-security-framework-sys-2.17.0
                              rust-semver-1.0.27
                              rust-serde-1.0.228
                              rust-serde-core-1.0.228
                              rust-serde-derive-1.0.228
                              rust-serde-json-1.0.149
                              rust-serde-spanned-1.0.4
                              rust-serde-urlencoded-0.7.1
                              rust-sha2-0.10.9
                              rust-sharded-slab-0.1.7
                              rust-shlex-1.3.0
                              rust-simd-adler32-0.3.8
                              rust-similar-2.7.0
                              rust-slab-0.4.12
                              rust-smallvec-1.15.1
                              rust-snapbox-1.0.0
                              rust-snapbox-macros-1.0.0
                              rust-socket2-0.6.2
                              rust-stable-deref-trait-1.2.1
                              rust-strsim-0.11.1
                              rust-subtle-2.6.1
                              rust-syn-2.0.117
                              rust-sync-wrapper-1.0.2
                              rust-synstructure-0.13.2
                              rust-sys-info-0.9.1
                              rust-tar-0.4.44
                              rust-tempfile-3.26.0
                              rust-terminal-size-0.4.3
                              rust-thiserror-1.0.69
                              rust-thiserror-2.0.18
                              rust-thiserror-impl-1.0.69
                              rust-thiserror-impl-2.0.18
                              rust-thread-local-1.1.9
                              rust-threadpool-1.8.1
                              rust-time-0.3.47
                              rust-time-core-0.1.8
                              rust-time-macros-0.2.27
                              rust-tinystr-0.8.2
                              rust-tokio-1.49.0
                              rust-tokio-macros-2.6.0
                              rust-tokio-native-tls-0.3.1
                              rust-tokio-retry-0.3.0
                              rust-tokio-rustls-0.26.4
                              rust-tokio-stream-0.1.18
                              rust-tokio-util-0.7.18
                              rust-toml-1.0.3+spec-1.1.0
                              rust-toml-datetime-1.0.0+spec-1.1.0
                              rust-toml-parser-1.0.9+spec-1.1.0
                              rust-toml-writer-1.0.6+spec-1.1.0
                              rust-tonic-0.14.5
                              rust-tonic-prost-0.14.5
                              rust-tower-0.5.3
                              rust-tower-http-0.6.8
                              rust-tower-layer-0.3.3
                              rust-tower-service-0.3.3
                              rust-tracing-0.1.44
                              rust-tracing-attributes-0.1.31
                              rust-tracing-core-0.1.36
                              rust-tracing-log-0.2.0
                              rust-tracing-opentelemetry-0.32.0
                              rust-tracing-subscriber-0.3.19
                              rust-try-lock-0.2.5
                              rust-typenum-1.19.0
                              rust-unarray-0.1.4
                              rust-unicase-2.9.0
                              rust-unicode-ident-1.0.24
                              rust-unicode-width-0.2.2
                              rust-unicode-xid-0.2.6
                              rust-unit-prefix-0.5.2
                              rust-untrusted-0.9.0
                              rust-url-2.5.8
                              rust-utf8-width-0.1.8
                              rust-utf8-iter-1.0.4
                              rust-utf8parse-0.2.2
                              rust-valuable-0.1.1
                              rust-vcpkg-0.2.15
                              rust-version-check-0.9.5
                              rust-wait-timeout-0.2.1
                              rust-walkdir-2.5.0
                              rust-want-0.3.1
                              rust-wasi-0.11.1+wasi-snapshot-preview1
                              rust-wasip2-1.0.2+wasi-0.2.9
                              rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
                              rust-wasm-bindgen-0.2.114
                              rust-wasm-bindgen-futures-0.4.64
                              rust-wasm-bindgen-macro-0.2.114
                              rust-wasm-bindgen-macro-support-0.2.114
                              rust-wasm-bindgen-shared-0.2.114
                              rust-wasm-encoder-0.244.0
                              rust-wasm-metadata-0.244.0
                              rust-wasm-streams-0.5.0
                              rust-wasmparser-0.244.0
                              rust-web-sys-0.3.91
                              rust-web-time-1.1.0
                              rust-webpki-root-certs-1.0.6
                              rust-winapi-0.3.9
                              rust-winapi-i686-pc-windows-gnu-0.4.0
                              rust-winapi-util-0.1.11
                              rust-winapi-x86-64-pc-windows-gnu-0.4.0
                              rust-windows-link-0.2.1
                              rust-windows-registry-0.6.1
                              rust-windows-result-0.4.1
                              rust-windows-strings-0.5.1
                              rust-windows-sys-0.45.0
                              rust-windows-sys-0.52.0
                              rust-windows-sys-0.59.0
                              rust-windows-sys-0.60.2
                              rust-windows-sys-0.61.2
                              rust-windows-targets-0.42.2
                              rust-windows-targets-0.52.6
                              rust-windows-targets-0.53.5
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
                              rust-winnow-0.7.14
                              rust-wit-bindgen-0.51.0
                              rust-wit-bindgen-core-0.51.0
                              rust-wit-bindgen-rust-0.51.0
                              rust-wit-bindgen-rust-macro-0.51.0
                              rust-wit-component-0.244.0
                              rust-wit-parser-0.244.0
                              rust-writeable-0.6.2
                              rust-xattr-1.6.1
                              rust-xz2-0.1.7
                              rust-yoke-0.8.1
                              rust-yoke-derive-0.8.1
                              rust-zerocopy-0.8.40
                              rust-zerocopy-derive-0.8.40
                              rust-zerofrom-0.1.6
                              rust-zerofrom-derive-0.1.6
                              rust-zeroize-1.8.2
                              rust-zerotrie-0.2.3
                              rust-zerovec-0.11.5
                              rust-zerovec-derive-0.11.2
                              rust-zlib-rs-0.6.2
                              rust-zmij-1.0.21
                              rust-zstd-0.13.3
                              rust-zstd-safe-7.2.4
                              rust-zstd-sys-2.0.16+zstd.1.5.7)))

(define rustup
  (package
    (name "rustup")
    (version "1.29.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/rust-lang/rustup/archive/refs/tags/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mdzz2l9rhck4jv8sn33kmnsl26wv0iwbcdxysi0jm2d5ykd2wyy"))))
    (arguments (list #:tests? #f))
    (build-system cargo-build-system)
    (inputs (cons* (list zstd "lib") openssl pkg-config (lookup-cargo-inputs 'rustup)))
    (home-page "https://www.rust-lang.org/")
    (synopsis "Rust language installer")
    (description "Rustup installs The Rust Programming Language from the official release channels, enabling you to easily switch between stable, beta, and nightly compilers and keep them updated. It makes cross-compiling simpler with binary builds of the standard library for common platforms. And it runs on all platforms Rust supports, including Windows.")
    (license (list license:asl2.0 license:expat))))

rustup

