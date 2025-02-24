(define-module (packages pulseshitter)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-audio)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-compression)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xiph)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix build-system cargo))

(define-public rust-reqwest-0.12
  (package
    (name "rust-reqwest")
    (version "0.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "reqwest" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "047aa0qnngnlnf9i0abrs6pgmz15vk81p5pvscwhk3l6jbfsyv2n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-compression" ,rust-async-compression-0.4)
                       ("rust-base64" ,rust-base64-0.22)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cookie" ,rust-cookie-0.17)
                       ("rust-cookie-store" ,rust-cookie-store-0.20)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-h2" ,rust-h2-0.4)
                       ("rust-h3" ,rust-h3-0.0.4)
                       ("rust-h3-quinn" ,rust-h3-quinn-0.0.5)
                       ("rust-hickory-resolver" ,rust-hickory-resolver-0.24)
                       ("rust-http" ,rust-http-1)
                       ("rust-http-body" ,rust-http-body-1)
                       ("rust-http-body-util" ,rust-http-body-util-0.1)
                       ("rust-hyper" ,rust-hyper-1)
                       ("rust-hyper-rustls" ,rust-hyper-rustls-0.26)
                       ("rust-hyper-tls" ,rust-hyper-tls-0.6)
                       ("rust-hyper-util" ,rust-hyper-util-0.1)
                       ("rust-ipnet" ,rust-ipnet-2)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-quinn" ,rust-quinn-0.10)
                       ("rust-rustls" ,rust-rustls-0.22)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.7)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
                       ("rust-sync-wrapper" ,rust-sync-wrapper-0.1)
                       ("rust-system-configuration" ,rust-system-configuration-0.5)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.25)
                       ("rust-tokio-socks" ,rust-tokio-socks-0.5)
                       ("rust-tokio-util" ,rust-tokio-util-0.7)
                       ("rust-tower-service" ,rust-tower-service-0.3)
                       ("rust-url" ,rust-url-2)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
                       ("rust-wasm-streams" ,rust-wasm-streams-0.4)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.26)
                       ("rust-winreg" ,rust-winreg-0.52))
       #:cargo-development-inputs (("rust-brotli" ,rust-brotli-3)
                                   ("rust-doc-comment" ,rust-doc-comment-0.3)
                                   ("rust-env-logger" ,rust-env-logger-0.10)
                                   ("rust-futures-util" ,rust-futures-util-0.3)
                                   ("rust-hyper" ,rust-hyper-1)
                                   ("rust-hyper-util" ,rust-hyper-util-0.1)
                                   ("rust-libflate" ,rust-libflate-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-tokio" ,rust-tokio-1)
                                   ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3)
                                   ("rust-zstd" ,rust-zstd-0.13))))
    (home-page "https://github.com/seanmonstar/reqwest")
    (synopsis "higher level HTTP client library")
    (description "This package provides higher level HTTP client library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ringbuf-0.3
  (package
    (name "rust-ringbuf")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ringbuf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02lmklwymawzfrgp9wy60yk2v3lkyv2p5v0w40la3lhzim1fvavr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8))))
    (home-page "https://github.com/agerasev/ringbuf.git")
    (synopsis
     "Lock-free SPSC FIFO ring buffer with direct access to inner data")
    (description
     "This package provides Lock-free SPSC FIFO ring buffer with direct access to inner data.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-derive-1
  (package
    (name "rust-serde-derive")
    (version "1.0.156")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z88gj1imji06pwll6il2qvcvx4mwzf2hci29b3wwsz30539rqnp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
     "This package provides Macros 1.1 implementation of #[derive(Serialize, Deserialize)].")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-1
  (package
    (name "rust-serde")
    (version "1.0.156")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19394in28sb9gh1v2153rqkyq46irr81x5a20701gpha5h4mnjri"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde-derive" ,rust-serde-derive-1))
       #:cargo-development-inputs (("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://serde.rs")
    (synopsis "generic serialization/deserialization framework")
    (description
     "This package provides a generic serialization/deserialization framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-json-1
  (package
    (name "rust-serde-json")
    (version "1.0.96")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1waj3qwpa610vmksnzcmkll6vaw7nf7v3ckj4v0wlfs0a153jz85"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-automod" ,rust-automod-1)
                                   ("rust-indoc" ,rust-indoc-2)
                                   ("rust-ref-cast" ,rust-ref-cast-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-stacker" ,rust-serde-stacker-0.1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "JSON serialization file format")
    (description "This package provides a JSON serialization file format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ron-0.8
  (package
    (name "rust-ron")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ron" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zvb2gxn4vv24swwp8a1l9fg5p960w9f9zd9ny05rd8w7c2m22ih"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-option-set" ,rust-option-set-0.1)
                                   ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/ron-rs/ron")
    (synopsis "Rusty Object Notation")
    (description "This package provides Rusty Object Notation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-0.8
  (package
    (name "rust-crossbeam")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b0s0ans81ja6gm7awlaw3k2rqywzmhq4mm9ra8yaak16q6sy098"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.9)
                       ("rust-crossbeam-queue" ,rust-crossbeam-queue-0.3)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8))
       #:cargo-development-inputs (("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/crossbeam-rs/crossbeam")
    (synopsis "Tools for concurrent programming")
    (description "This package provides tools for concurrent programming.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zeroize-1
  (package
    (name "rust-zeroize")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zeroize" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z8yix823b6lz878qwg6bvwhg3lb0cbw3c9yij9p8mbv7zdzfmj7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-zeroize-derive" ,rust-zeroize-derive-1))))
    (home-page "https://github.com/RustCrypto/utils/tree/master/zeroize")
    (synopsis "Securely clear secrets from memory with a simple trait built on
stable Rust primitives which guarantee memory is zeroed using an
operation will not be 'optimized away' by the compiler.
Uses a portable pure Rust implementation that works everywhere,
even WASM!")
    (description
     "This package provides Securely clear secrets from memory with a simple trait built on stable Rust
primitives which guarantee memory is zeroed using an operation will not be
optimized away by the compiler.  Uses a portable pure Rust implementation that
works everywhere, even WASM!")
    (license (list license:asl2.0 license:expat))))

(define-public rust-subtle-2
  (package
    (name "rust-subtle")
    (version "2.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "subtle" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00b6jzh9gzb0h9n25g06nqr90z3xzqppfhhb260s1hjhh4pg7pkb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://dalek.rs/")
    (synopsis
     "Pure-Rust traits and utilities for constant-time cryptographic implementations")
    (description
     "This package provides Pure-Rust traits and utilities for constant-time cryptographic implementations.")
    (license license:bsd-3)))

(define-public rust-xsalsa20poly1305-0.8
  (package
    (name "rust-xsalsa20poly1305")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "xsalsa20poly1305" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11h87km1621v8gwy458wl2cx5p075yhwx58b8n8h0rbcbnbcp2z6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aead" ,rust-aead-0.4)
                       ("rust-poly1305" ,rust-poly1305-0.7)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-salsa20" ,rust-salsa20-0.9)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/AEADs")
    (synopsis "DEPRECATED: please use the `crypto_secretbox` crate")
    (description
     "This package provides DEPRECATED: please use the `crypto_secretbox` crate.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-twilight-validate-0.12
  (package
    (name "rust-twilight-validate")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "twilight-validate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qai12zy61f5kf9hgjl0a1m6s99jp7p9nfjnf5cyp2qdbc3z00p0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-twilight-model" ,rust-twilight-model-0.12))))
    (home-page "https://twilight.rs/")
    (synopsis "Functions and constants for validating request parameters")
    (description
     "This package provides functions and constants for validating request parameters.")
    (license license:isc)))

(define-public rust-twilight-model-0.12
  (package
    (name "rust-twilight-model")
    (version "0.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "twilight-model" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yipkmx2qz2h167jp199i58izhmib8gv1bdbxgm1bdxsagsh9i3c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-value" ,rust-serde-value-0.7)
                       ("rust-serde-repr" ,rust-serde-repr-0.1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://twilight.rs/chapter_1_crates/section_1_model.html")
    (synopsis "Discord API models for the Twilight ecosystem")
    (description
     "This package provides Discord API models for the Twilight ecosystem.")
    (license license:isc)))

(define-public rust-twilight-http-ratelimiting-0.12
  (package
    (name "rust-twilight-http-ratelimiting")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "twilight-http-ratelimiting" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00dvvv0f99235lphz7zpj0nf9aqmlq9fy6c5f65v2q1133aqymvr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://twilight.rs/")
    (synopsis
     "Discord REST API ratelimiter implementations for the Twilight ecosystem")
    (description
     "This package provides Discord REST API ratelimiter implementations for the Twilight ecosystem.")
    (license license:isc)))

(define-public rust-rustls-pemfile-0.3
  (package
    (name "rust-rustls-pemfile")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-pemfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q3k136sna6yhq98js7n7lf341w47j6gxzin2lfncz1ajxinvs0y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13))))
    (home-page "https://github.com/rustls/pemfile")
    (synopsis "Basic .pem file parser for keys and certificates")
    (description
     "This package provides Basic .pem file parser for keys and certificates.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-enum-as-inner-0.4
  (package
    (name "rust-enum-as-inner")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "enum-as-inner" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wwvjmy2bvqqc3pyylsmlqpkswxrzsg40xva7z27szva8j0svk91"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/bluejekyll/enum-as-inner")
    (synopsis
     "proc-macro for deriving inner field accessor functions on enums.")
    (description
     "This package provides a proc-macro for deriving inner field accessor functions
on enums.")
    (license (list license:expat license:asl2.0))))

(define-public rust-trust-dns-proto-0.21
  (package
    (name "rust-trust-dns-proto")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-proto" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0p95ig8dfp30ga6gz01m683zy33abbna0givpgac6xwqym0g4ccw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-enum-as-inner" ,rust-enum-as-inner-0.4)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-h2" ,rust-h2-0.3)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-idna" ,rust-idna-0.2)
                       ("rust-ipnet" ,rust-ipnet-2)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-socket2" ,rust-socket2-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tinyvec" ,rust-tinyvec-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-url" ,rust-url-2)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-webpki" ,rust-webpki-0.22)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://trust-dns.org/")
    (synopsis
     "Trust-DNS is a safe and secure DNS library. This is the foundational DNS protocol library for all Trust-DNS projects.")
    (description
     "This package provides Trust-DNS is a safe and secure DNS library.  This is the foundational DNS
protocol library for all Trust-DNS projects.")
    (license (list license:expat license:asl2.0))))

(define-public rust-trust-dns-resolver-0.21
  (package
    (name "rust-trust-dns-resolver")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-resolver" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n6m9yvhaip8dml5247d6qqdzf8bcrn4rvzwr685clc4xb175fp4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-ipconfig" ,rust-ipconfig-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-lru-cache" ,rust-lru-cache-0.1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-resolv-conf" ,rust-resolv-conf-0.7)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-trust-dns-proto" ,rust-trust-dns-proto-0.21)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://trust-dns.org/")
    (synopsis
     "Trust-DNS is a safe and secure DNS library. This Resolver library  uses the Client library to perform all DNS queries. The Resolver is intended to be a high-level library for any DNS record resolution see Resolver and AsyncResolver for supported resolution types. The Client can be used for other queries.")
    (description
     "This package provides Trust-DNS is a safe and secure DNS library.  This Resolver library uses the
Client library to perform all DNS queries.  The Resolver is intended to be a
high-level library for any DNS record resolution see Resolver and
@code{AsyncResolver} for supported resolution types.  The Client can be used for
other queries.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hyper-trust-dns-0.4
  (package
    (name "rust-hyper-trust-dns")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-trust-dns" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09xzwa3irqk7ng4wd70vpvvm2if88gkzfkra2npw69sxh8q8ngsm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-hyper" ,rust-hyper-0.14)
                       ("rust-hyper-rustls" ,rust-hyper-rustls-0.23)
                       ("rust-hyper-tls" ,rust-hyper-tls-0.5)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-trust-dns-resolver" ,rust-trust-dns-resolver-0.21))))
    (home-page "https://github.com/Gelbpunkt/hyper-trust-dns")
    (synopsis
     "HTTP/HTTPS connectors for hyper that use trust-dns' DNS resolver")
    (description
     "This package provides HTTP/HTTPS connectors for hyper that use trust-dns DNS resolver.")
    (license license:expat)))

(define-public rust-twilight-http-0.12
  (package
    (name "rust-twilight-http")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "twilight-http" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16d3xq6lksm5df9a3k0rgms2hd9bc0kgvhf9v8ah81smdxgm4a9k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-brotli" ,rust-brotli-3)
                       ("rust-hyper" ,rust-hyper-0.14)
                       ("rust-hyper-rustls" ,rust-hyper-rustls-0.23)
                       ("rust-hyper-tls" ,rust-hyper-tls-0.5)
                       ("rust-hyper-trust-dns" ,rust-hyper-trust-dns-0.4)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-simd-json" ,rust-simd-json-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-twilight-http-ratelimiting" ,rust-twilight-http-ratelimiting-0.12)
                       ("rust-twilight-model" ,rust-twilight-model-0.12)
                       ("rust-twilight-validate" ,rust-twilight-validate-0.12))))
    (home-page "https://twilight.rs/chapter_1_crates/section_2_http.html")
    (synopsis "Discord REST API client for the Twilight ecosystem")
    (description
     "This package provides Discord REST API client for the Twilight ecosystem.")
    (license license:isc)))

(define-public rust-twilight-gateway-queue-0.12
  (package
    (name "rust-twilight-gateway-queue")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "twilight-gateway-queue" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02rjxf4a71c5n95par2mq0b6l6cy61a0anfml3z2zpayw8191fxc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-twilight-http" ,rust-twilight-http-0.12))))
    (home-page "https://twilight.rs/")
    (synopsis
     "Discord Gateway connection queue implementation for the Twilight ecosystem")
    (description
     "This package provides Discord Gateway connection queue implementation for the Twilight ecosystem.")
    (license license:isc)))

(define-public rust-tokio-tungstenite-0.17
  (package
    (name "rust-tokio-tungstenite")
    (version "0.17.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10dingfgq7ch65dzv2j0q8k3ghdf3ihl6hp0fwfl145dpqaxs57p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-tungstenite" ,rust-tungstenite-0.17)
                       ("rust-webpki" ,rust-webpki-0.22)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/snapview/tokio-tungstenite")
    (synopsis
     "Tokio binding for Tungstenite, the Lightweight stream-based WebSocket implementation")
    (description
     "This package provides Tokio binding for Tungstenite, the Lightweight stream-based @code{WebSocket}
implementation.")
    (license license:expat)))

(define-public rust-value-trait-0.4
  (package
    (name "rust-value-trait")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "value-trait" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13scxczav8ha0cfbgj67z1wh28akh4hgsbsdbq96xdj9fr03b9n0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-abi-stable" ,rust-abi-stable-0.10)
                       ("rust-float-cmp" ,rust-float-cmp-0.9)
                       ("rust-halfbrown" ,rust-halfbrown-0.1)
                       ("rust-hashbrown" ,rust-hashbrown-0.12)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-ryu" ,rust-ryu-1))))
    (home-page "https://github.com/simd-lite/value-trait")
    (synopsis "Traits to deal with JSONesque values")
    (description "This package provides Traits to deal with JSONesque values.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-simd-json-0.6
  (package
    (name "rust-simd-json")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "simd-json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "110z93jy8f4dvc42dly3x1l9z123cfvhjafpyyilzrlx1f28pmwv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-alloc-counter" ,rust-alloc-counter-0.0.4)
                       ("rust-beef" ,rust-beef-0.5)
                       ("rust-colored" ,rust-colored-2)
                       ("rust-getopts" ,rust-getopts-0.2)
                       ("rust-halfbrown" ,rust-halfbrown-0.1)
                       ("rust-jemallocator" ,rust-jemallocator-0.5)
                       ("rust-perfcnt" ,rust-perfcnt-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-simdutf8" ,rust-simdutf8-0.1)
                       ("rust-value-trait" ,rust-value-trait-0.4))))
    (home-page "https://github.com/simd-lite/simd-json")
    (synopsis "High performance JSON parser based on a port of simdjson")
    (description
     "This package provides High performance JSON parser based on a port of simdjson.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-metrics-macros-0.5
  (package
    (name "rust-metrics-macros")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "metrics-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yjzi989pmjiixz3pwj95jrdpisdla4h7r91rzjpnx9z149hiqs9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "Macros for the metrics crate")
    (description "This package provides Macros for the metrics crate.")
    (license license:expat)))

(define-public rust-metrics-0.19
  (package
    (name "rust-metrics")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "metrics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wih61sm996rmiflvdcmxn4166py9afs15c222a8vdi3a6456b0l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ahash" ,rust-ahash-0.7)
                       ("rust-metrics-macros" ,rust-metrics-macros-0.5))))
    (home-page "https://github.com/metrics-rs/metrics")
    (synopsis "lightweight metrics facade.")
    (description "This package provides a lightweight metrics facade.")
    (license license:expat)))

(define-public rust-leaky-bucket-lite-0.5
  (package
    (name "rust-leaky-bucket-lite")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "leaky-bucket-lite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0aajpyc2qjc6v5nxz13j4z62s2wjgx5z36mj9824i9r1vlvwf48l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/Gelbpunkt/leaky-bucket-lite")
    (synopsis "Slimmed down, lazy futures-aware rate limiter implementation.")
    (description
     "This package provides Slimmed down, lazy futures-aware rate limiter implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-twilight-gateway-0.12
  (package
    (name "rust-twilight-gateway")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "twilight-gateway" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0373yighd7laqb2b40w4sm1cfm6k0v0dd0178wn1gg2bzr2ww0rf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-leaky-bucket-lite" ,rust-leaky-bucket-lite-0.5)
                       ("rust-metrics" ,rust-metrics-0.19)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-simd-json" ,rust-simd-json-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.17)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-twilight-gateway-queue" ,rust-twilight-gateway-queue-0.12)
                       ("rust-twilight-http" ,rust-twilight-http-0.12)
                       ("rust-twilight-model" ,rust-twilight-model-0.12)
                       ("rust-url" ,rust-url-2)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://twilight.rs/chapter_1_crates/section_3_gateway.html")
    (synopsis "Discord Gateway implementation for the Twilight ecosystem")
    (description
     "This package provides Discord Gateway implementation for the Twilight ecosystem.")
    (license license:isc)))

(define-public rust-symphonia-core-0.5
  (package
    (name "rust-symphonia-core")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "symphonia-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hr2w2a217vq4lpghszmsdwxr5ilh5d1ysfm3cixbirxkrvhd0vr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustfft" ,rust-rustfft-6))))
    (home-page "https://github.com/pdeljanov/Symphonia")
    (synopsis "Project Symphonia shared structs, traits, and features")
    (description
     "This package provides Project Symphonia shared structs, traits, and features.")
    (license license:mpl2.0)))

(define-public rust-streamcatcher-1
  (package
    (name "rust-streamcatcher")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "streamcatcher" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0agazjs1sja5n41jqc7rfcmcm4935pxihqnsixsv1as9qdalfrki"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-compat" ,rust-async-compat-0.2)
                       ("rust-async-std" ,rust-async-std-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-loom" ,rust-loom-0.5)
                       ("rust-smol" ,rust-smol-1)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/FelixMcFelix/streamcatcher")
    (synopsis
     "thread-safe, shared (asynchronous), almost-lockless stream buffer.")
    (description
     "This package provides a thread-safe, shared (asynchronous), almost-lockless
stream buffer.")
    (license (list license:expat license:asl2.0))))

(define-public rust-uwl-0.6
  (package
    (name "rust-uwl")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "uwl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l0spdpn879wpf440x4cdsbz5dilp5ihfsxsqkn2dmkhrbh07gzl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/acdenisSK/uwl")
    (synopsis "management stream for bytes and characters")
    (description
     "This package provides a management stream for bytes and characters.")
    (license (list license:expat license:asl2.0))))

(define-public rust-typemap-rev-0.1
  (package
    (name "rust-typemap-rev")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "typemap_rev" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mg1bljnddkwh8ahy1xq1bgrmc4k8fcvdavr19c58m2blbq78nzd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/bdashore3/typemap_rev")
    (synopsis "hashmap, but stores types as keys")
    (description "This package provides a hashmap, but stores types as keys.")
    (license license:isc)))

(define-public rust-value-trait-0.2
  (package
    (name "rust-value-trait")
    (version "0.2.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "value-trait" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zb2cvwkdvd6v80z9h7rd61b6sp57a61q0pgd03b2lphlrs0mr0g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-abi-stable" ,rust-abi-stable-0.10)
                       ("rust-float-cmp" ,rust-float-cmp-0.9)
                       ("rust-halfbrown" ,rust-halfbrown-0.1)
                       ("rust-itoa" ,rust-itoa-0.4)
                       ("rust-ryu" ,rust-ryu-1))))
    (home-page "https://github.com/simd-lite/value-trait")
    (synopsis "Traits to deal with JSONesque values")
    (description "This package provides Traits to deal with JSONesque values.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-x86-0.47
  (package
    (name "rust-x86")
    (version "0.47.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x86" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jlddyczw168mcy4a6m3nbl203rxli2vr5gcmf57s0adqf6bxdam"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bit-field" ,rust-bit-field-0.10)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-csv" ,rust-csv-1)
                       ("rust-phf" ,rust-phf-0.9)
                       ("rust-phf-codegen" ,rust-phf-codegen-0.9)
                       ("rust-raw-cpuid" ,rust-raw-cpuid-10)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/gz/rust-x86")
    (synopsis
     "Library to program x86 (amd64) hardware. Contains x86 specific data structure descriptions, data-tables, as well as convenience function to call assembly instructions typically not exposed in higher level languages")
    (description
     "This package provides Library to program x86 (amd64) hardware.  Contains x86 specific data structure
descriptions, data-tables, as well as convenience function to call assembly
instructions typically not exposed in higher level languages.")
    (license license:expat)))

(define-public rust-libc-0.1
  (package
    (name "rust-libc")
    (version "0.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08k14zb7bw25avmaj227calcdglb4ac394kklr9nv175fp7p0ap3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.")
    (description
     "This package provides Raw FFI bindings to platform libraries like libc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mmap-0.1
  (package
    (name "rust-mmap")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08xqhvr4l3rf1fkz2w4cwz3z5wd0m1jab1d34sxd4v80lr459j0b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.1)
                       ("rust-tempdir" ,rust-tempdir-0.3))))
    (home-page "https://github.com/rbranson/rust-mmap")
    (synopsis "library for dealing with memory-mapped I/O")
    (description
     "This package provides a library for dealing with memory-mapped I/O.")
    (license license:expat)))

(define-public rust-perfcnt-0.8
  (package
    (name "rust-perfcnt")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "perfcnt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "008mrdd8zjk54qg8xh8crk9is98sxv2c0kk2v25nzjkhaaazv8ab"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mmap" ,rust-mmap-0.1)
                       ("rust-nom" ,rust-nom-4)
                       ("rust-x86" ,rust-x86-0.47))))
    (home-page "https://github.com/gz/rust-perfcnt")
    (synopsis
     "Library to configure and read hardware performance counters in rust")
    (description
     "This package provides Library to configure and read hardware performance counters in rust.")
    (license license:expat)))

(define-public rust-halfbrown-0.1
  (package
    (name "rust-halfbrown")
    (version "0.1.18")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "halfbrown" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ks4hpw8xfk2lk4ja6rzsqbvnwsza3wqjkmmhzpc2360m5q3qaly"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-fxhash" ,rust-fxhash-0.2)
                       ("rust-hashbrown" ,rust-hashbrown-0.13)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Licenser/halfbrown")
    (synopsis
     "Multi backend HashMap for higher performance on different key space sizes")
    (description
     "This package provides Multi backend @code{HashMap} for higher performance on different key space
sizes.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-alloc-counter-macro-0.0.2
  (package
    (name "rust-alloc-counter-macro")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "alloc_counter_macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nifqalryavmrdlkyv7cznp8yfjj16x0bjqzvjndw0fxk8gzhlhs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "")
    (synopsis "The #[no_alloc] macro for the alloc_counter crate")
    (description
     "This package provides The #[no_alloc] macro for the alloc_counter crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-simd-json-0.4
  (package
    (name "rust-simd-json")
    (version "0.4.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "simd-json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m34gwia6mrpr1s8sf2gbf126n0d5xr9dyiyhka6wqwvfaksmhy0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-alloc-counter" ,rust-alloc-counter-0.0.4)
                       ("rust-beef" ,rust-beef-0.5)
                       ("rust-colored" ,rust-colored-2)
                       ("rust-core-affinity" ,rust-core-affinity-0.5)
                       ("rust-criterion" ,rust-criterion-0.3)
                       ("rust-getopts" ,rust-getopts-0.2)
                       ("rust-halfbrown" ,rust-halfbrown-0.1)
                       ("rust-jemallocator" ,rust-jemallocator-0.3)
                       ("rust-perfcnt" ,rust-perfcnt-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-simdutf8" ,rust-simdutf8-0.1)
                       ("rust-value-trait" ,rust-value-trait-0.2))))
    (home-page "https://github.com/simd-lite/simd-json")
    (synopsis "High performance JSON parser based on a port of simdjson")
    (description
     "This package provides High performance JSON parser based on a port of simdjson.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-serenity-voice-model-0.1
  (package
    (name "rust-serenity-voice-model")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serenity-voice-model" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lj93n0wx546glkvwgr8yjsyjs5xdzcvxpx5x3hzv8lwhk4axqwb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-enum-primitive" ,rust-enum-primitive-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-repr" ,rust-serde-repr-0.1))))
    (home-page "https://github.com/serenity-rs/serenity")
    (synopsis
     "Rust library for (de)serializing Discord Voice API gateway messages.")
    (description
     "This package provides a Rust library for (de)serializing Discord Voice API
gateway messages.")
    (license license:isc)))

(define-public rust-prost-types-0.11
  (package
    (name "rust-prost-types")
    (version "0.11.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "prost-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04ryk38sqkp2nf4dgdqdfbgn6zwwvjraw6hqq6d9a6088shj4di1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-prost" ,rust-prost-0.11))))
    (home-page "https://github.com/tokio-rs/prost")
    (synopsis "Prost definitions of Protocol Buffers well known types")
    (description
     "This package provides Prost definitions of Protocol Buffers well known types.")
    (license license:asl2.0)))

(define-public rust-quanta-0.11
  (package
    (name "rust-quanta")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quanta" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1axrw0nqc90bq671w05jd9460pmwg86c4r132mjsi4c2g8m6czm1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mach2" ,rust-mach2-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-prost-types" ,rust-prost-types-0.11)
                       ("rust-raw-cpuid" ,rust-raw-cpuid-10)
                       ("rust-wasi" ,rust-wasi-0.11)
                       ("rust-web-sys" ,rust-web-sys-0.3)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/metrics-rs/quanta")
    (synopsis "high-speed timing library")
    (description "This package provides high-speed timing library.")
    (license license:expat)))

(define-public rust-moka-0.9
  (package
    (name "rust-moka")
    (version "0.9.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "moka" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "195fzbphmz2rbxi2m8yv56f02s7iszxrqzja0mv601338fn5b15j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-io" ,rust-async-io-1)
                       ("rust-async-lock" ,rust-async-lock-2)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.9)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-dashmap" ,rust-dashmap-5)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-quanta" ,rust-quanta-0.11)
                       ("rust-rustc-version" ,rust-rustc-version-0.4)
                       ("rust-scheduled-thread-pool" ,rust-scheduled-thread-pool-0.2)
                       ("rust-skeptic" ,rust-skeptic-0.13)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-tagptr" ,rust-tagptr-0.2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-triomphe" ,rust-triomphe-0.1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/moka-rs/moka")
    (synopsis "fast and concurrent cache library inspired by Java Caffeine")
    (description
     "This package provides a fast and concurrent cache library inspired by Java
Caffeine.")
    (license (list license:expat license:asl2.0))))

(define-public rust-command-attr-0.4
  (package
    (name "rust-command-attr")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "command_attr" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r4qs6a056xb46ajivlk1l9dcq8sglab9cilki6ds1lqkg8qgdq7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "")
    (synopsis
     "Procedural macros for command creation for the Serenity library")
    (description
     "This package provides Procedural macros for command creation for the Serenity library.")
    (license license:isc)))

(define-public rust-serenity-0.11
  (package
    (name "rust-serenity")
    (version "0.11.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serenity" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ii2js99shrpsammck58mn5f2a1rdlgg8bdg9jfzr0rlyb78jyks"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-async-tungstenite" ,rust-async-tungstenite-0.17)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-command-attr" ,rust-command-attr-0.4)
                       ("rust-dashmap" ,rust-dashmap-5)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-levenshtein" ,rust-levenshtein-1)
                       ("rust-mime" ,rust-mime-0.3)
                       ("rust-mime-guess" ,rust-mime-guess-2)
                       ("rust-moka" ,rust-moka-0.9)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-value" ,rust-serde-value-0.7)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serenity-voice-model" ,rust-serenity-voice-model-0.1)
                       ("rust-simd-json" ,rust-simd-json-0.4)
                       ("rust-static-assertions" ,rust-static-assertions-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-typemap-rev" ,rust-typemap-rev-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-uwl" ,rust-uwl-0.6))))
    (home-page "https://github.com/serenity-rs/serenity")
    (synopsis "Rust library for the Discord API.")
    (description "This package provides a Rust library for the Discord API.")
    (license license:isc)))

(define-public rust-pnet-base-0.28
  (package
    (name "rust-pnet-base")
    (version "0.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pnet_base" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f3g2p8zgmgsgyzjai4mv0nl22k1lxlrzz7zlsmdqlx7a7aqqj15"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/libpnet/libpnet")
    (synopsis "Fundamental base types and code used by pnet")
    (description
     "This package provides Fundamental base types and code used by pnet.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pnet-macros-support-0.28
  (package
    (name "rust-pnet-macros-support")
    (version "0.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pnet_macros_support" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "144gxxamd1904azjlrlky324mp9h5n7h9kmd0lq05aqcyc84wwfl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pnet-base" ,rust-pnet-base-0.28))))
    (home-page "https://github.com/libpnet/libpnet")
    (synopsis "Support library for libpnet_macros")
    (description "This package provides Support library for libpnet_macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pnet-macros-0.28
  (package
    (name "rust-pnet-macros")
    (version "0.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pnet_macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j3mslgg6c1fdgsnqk9s0a9z894an2brilz0zaw05175a840wj9h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/libpnet/libpnet")
    (synopsis "Automatic bit manipulation for binary data formats")
    (description
     "This package provides Automatic bit manipulation for binary data formats.")
    (license (list license:expat license:asl2.0))))

(define-public rust-discortp-0.4
  (package
    (name "rust-discortp")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "discortp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wk9s8jm32c0b1sysh8sgxj2z5dmig0kmmihxasj10m48rv02rpv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pnet-macros" ,rust-pnet-macros-0.28)
                       ("rust-pnet-macros-support" ,rust-pnet-macros-support-0.28))))
    (home-page "https://github.com/FelixMcFelix/discortp")
    (synopsis
     "Lightweight, flexible Real-time Transport Protocol (RTP) parsing library")
    (description
     "This package provides Lightweight, flexible Real-time Transport Protocol (RTP) parsing library.")
    (license license:isc)))

(define-public rust-audiopus-sys-0.2
  (package
    (name "rust-audiopus-sys")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "audiopus_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lc6kzdw65kbgqaghig99f8642k2ikl5imk56q1lw1m28qallcb2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.58)
                       ("rust-cmake" ,rust-cmake-0.1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/lakelezz/audiopus_sys.git")
    (synopsis
     "FFI-Binding to Opus, dynamically or statically linked for Windows and UNIX")
    (description
     "This package provides FFI-Binding to Opus, dynamically or statically linked for Windows and UNIX.")
    (license license:isc)))

(define-public rust-audiopus-0.3
  (package
    (name "rust-audiopus")
    (version "0.3.0-rc.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "audiopus" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m9r8zk3n7r4x1p2fsmy6gn2axrd2bdyai7mb4yxxinpaq7fnmdb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-audiopus-sys" ,rust-audiopus-sys-0.2))))
    (home-page "https://github.com/lakelezz/audiopus.git")
    (synopsis "High-level binding of the Opus Codec library")
    (description
     "This package provides High-level binding of the Opus Codec library.")
    (license license:isc)))

(define-public rust-tungstenite-0.17
  (package
    (name "rust-tungstenite")
    (version "0.17.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q2czb80xb7hp7ipqi5d21716i52k8s7iz18xxzfwaccdbyr4yg2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-http" ,rust-http-0.2)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rustls" ,rust-rustls-0.20)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-sha-1" ,rust-sha-1-0.10)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2)
                       ("rust-utf-8" ,rust-utf-8-0.7)
                       ("rust-webpki" ,rust-webpki-0.22)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/snapview/tungstenite-rs")
    (synopsis "Lightweight stream-based WebSocket implementation")
    (description
     "This package provides Lightweight stream-based @code{WebSocket} implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-async-tls-0.11
  (package
    (name "rust-async-tls")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0044i87n6hyrdkmnlb5cs5iw9x04shd7nvhmymfqv0zivdlxf8rg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-rustls" ,rust-rustls-0.19)
                       ("rust-webpki" ,rust-webpki-0.21)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.21))))
    (home-page "https://github.com/async-std/async-tls")
    (synopsis "Asynchronous TLS/SSL streams using Rustls")
    (description
     "This package provides Asynchronous TLS/SSL streams using Rustls.")
    (license (list license:expat license:asl2.0))))

(define-public rust-async-native-tls-0.4
  (package
    (name "rust-async-native-tls")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-native-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zhkka5azpr03wg2bswabmwcwcqbdia17h2d17hk4wk47kn4qzfm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://docs.rs/crate/async-native-tls/")
    (synopsis "Native TLS using futures")
    (description "This package provides Native TLS using futures.")
    (license (list license:expat license:asl2.0))))

(define-public rust-async-tungstenite-0.17
  (package
    (name "rust-async-tungstenite")
    (version "0.17.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-tungstenite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fzc5f5zsyxxyk2r88p582lb2fm89wlgxvzkgn7alhqnaqqipdx1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-native-tls" ,rust-async-native-tls-0.4)
                       ("rust-async-std" ,rust-async-std-1)
                       ("rust-async-tls" ,rust-async-tls-0.11)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-gio" ,rust-gio-0.15)
                       ("rust-glib" ,rust-glib-0.15)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-native-tls" ,rust-tokio-native-tls-0.3)
                       ("rust-tokio-openssl" ,rust-tokio-openssl-0.6)
                       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
                       ("rust-tungstenite" ,rust-tungstenite-0.17)
                       ("rust-webpki-roots" ,rust-webpki-roots-0.22))))
    (home-page "https://github.com/sdroege/async-tungstenite")
    (synopsis
     "Async binding for Tungstenite, the Lightweight stream-based WebSocket implementation")
    (description
     "This package provides Async binding for Tungstenite, the Lightweight stream-based @code{WebSocket}
implementation.")
    (license license:expat)))

(define-public rust-songbird-0.3
  (package
    (name "rust-songbird")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "songbird" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "127jvs5z2b2ph13bjw2pb7hsz7qn8pp3rr533pg3j6bpznh8dxij"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-async-tungstenite" ,rust-async-tungstenite-0.17)
                       ("rust-audiopus" ,rust-audiopus-0.3)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-dashmap" ,rust-dashmap-5)
                       ("rust-derivative" ,rust-derivative-2)
                       ("rust-discortp" ,rust-discortp-0.4)
                       ("rust-flume" ,rust-flume-0.10)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-pin-project" ,rust-pin-project-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serenity" ,rust-serenity-0.11)
                       ("rust-serenity-voice-model" ,rust-serenity-voice-model-0.1)
                       ("rust-streamcatcher" ,rust-streamcatcher-1)
                       ("rust-symphonia-core" ,rust-symphonia-core-0.5)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-futures" ,rust-tracing-futures-0.2)
                       ("rust-twilight-gateway" ,rust-twilight-gateway-0.12)
                       ("rust-twilight-model" ,rust-twilight-model-0.12)
                       ("rust-typemap-rev" ,rust-typemap-rev-0.1)
                       ("rust-url" ,rust-url-2)
                       ("rust-uuid" ,rust-uuid-0.8)
                       ("rust-xsalsa20poly1305" ,rust-xsalsa20poly1305-0.8))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3))))
    (home-page "https://github.com/serenity-rs/songbird")
    (synopsis "An async Rust library for the Discord voice API")
    (description
     "This package provides An async Rust library for the Discord voice API.")
    (license license:isc)))

(define-public rust-alloc-counter-0.0.4
  (package
    (name "rust-alloc-counter")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "alloc_counter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1agxzprqi37bcy9hh3clbsl3n0awbb34vrlv4rp5afib8w53m31s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-alloc-counter-macro" ,rust-alloc-counter-macro-0.0.2)
                       ("rust-pin-utils" ,rust-pin-utils-0.1))))
    (home-page "https://gitlab.com/sio4/code/alloc-counter")
    (synopsis
     "Count allocations, reallocations, deallocations. Allow, deny, or forbid allocations on an expression or function basis")
    (description
     "This package provides Count allocations, reallocations, deallocations.  Allow, deny, or forbid
allocations on an expression or function basis.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mio-aio-0.7
  (package
    (name "rust-mio-aio")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mio-aio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07y7wnkd4xvj48fyxgnfm9jqjldgqfd39k3ydfmaqc76mw36pagq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-mio" ,rust-mio-0.8)
                       ("rust-nix" ,rust-nix-0.24))))
    (home-page "https://github.com/asomers/mio-aio")
    (synopsis "POSIX AIO bindings for mio")
    (description "This package provides POSIX AIO bindings for mio.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tokio-1
  (package
    (name "rust-tokio")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r3gnz0zh669q9jm7xh8dz7irbnxjddbbrfflp42jyn2qc0is803"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
                       ("rust-socket2" ,rust-socket2-0.4)
                       ("rust-tokio-macros" ,rust-tokio-macros-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-windows-sys" ,rust-windows-sys-0.45))
       #:cargo-development-inputs (("rust-async-stream" ,rust-async-stream-0.3)
                                   ("rust-futures" ,rust-futures-0.3)
                                   ("rust-libc" ,rust-libc-0.2)
                                   ("rust-loom" ,rust-loom-0.5)
                                   ("rust-mio-aio" ,rust-mio-aio-0.7)
                                   ("rust-mockall" ,rust-mockall-0.11)
                                   ("rust-nix" ,rust-nix-0.26)
                                   ("rust-ntapi" ,rust-ntapi-0.3)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-socket2" ,rust-socket2-0.4)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                                   ("rust-tokio-test" ,rust-tokio-test-0.4)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://tokio.rs")
    (synopsis
     "An event-driven, non-blocking I/O platform for writing asynchronous I/O
backed applications.")
    (description
     "This package provides An event-driven, non-blocking I/O platform for writing asynchronous I/O backed
applications.")
    (license license:expat)))

(define-public rust-libpulse-binding-2
  (package
    (name "rust-libpulse-binding")
    (version "2.28.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libpulse-binding" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zza12f22wf1qs6h71lq1i73aj3kmv3036hqc7qci063vyi5fdgd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libpulse-sys" ,rust-libpulse-sys-1)
        ("rust-num-derive" ,rust-num-derive-0.3)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-winapi" ,rust-winapi-0.3))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list pulseaudio))
    (home-page "https://github.com/jnqnfe/pulse-binding-rust")
    (synopsis "Binding for the PulseAudio libpulse library")
    (description
     "This package provides a Rust language binding for the PulseAudio libpulse
library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tui-0.19
  (package
    (name "rust-tui")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tui" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ldswnqgmdkd2fkislyh1amd6rmnbx3s8b97k9j7w03lsv5jdpfc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cassowary" ,rust-cassowary-0.3)
                       ("rust-crossterm" ,rust-crossterm-0.25)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-termion" ,rust-termion-1)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))
       #:cargo-development-inputs (("rust-argh" ,rust-argh-0.1)
                                   ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/fdehau/tui-rs")
    (synopsis "library to build rich terminal user interfaces or dashboards")
    (description
     "This package provides a library to build rich terminal user interfaces or
dashboards.")
    (license license:expat)))

(define-public rust-tui-textarea-0.2
  (package
    (name "rust-tui-textarea")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tui-textarea" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1biwnraf8y51lc7v2gn8d2n3lnpzw7nm2vxiv8qp4vynaxxdjyj3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-crossterm" ,rust-crossterm-0.25)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-termion" ,rust-termion-1)
                       ("rust-tui" ,rust-tui-0.19))))
    (home-page "https://github.com/rhysd/tui-textarea#readme")
    (synopsis
     "tui-textarea is a simple yet powerful text editor widget for ratatui and tui-rs. Multi-line
text editor can be easily put as part of your TUI application.")
    (description
     "This package provides tui-textarea is a simple yet powerful text editor widget for ratatui and tui-rs.
 Multi-line text editor can be easily put as part of your TUI application.")
    (license license:expat)))

(define-public rust-crossterm-0.25
  (package
    (name "rust-crossterm")
    (version "0.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossterm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rsbkkhdf61aipc06b7vpl4cw3wnxz0miizp0ms3a5rcpq7nqkp6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-crossterm-winapi" ,rust-crossterm-winapi-0.9)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-signal-hook-mio" ,rust-signal-hook-mio-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-async-std" ,rust-async-std-1)
                                   ("rust-futures" ,rust-futures-0.3)
                                   ("rust-futures-timer" ,rust-futures-timer-3)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/crossterm-rs/crossterm")
    (synopsis "crossplatform terminal library for manipulating terminals.")
    (description
     "This package provides a crossplatform terminal library for manipulating
terminals.")
    (license license:expat)))

(define-public rust-enum-iterator-derive-1
  (package
    (name "rust-enum-iterator-derive")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "enum-iterator-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nz6kz8jz2w1vy4y3r0mb8pa5nj3y77mdxdn3b38db322cf9kax1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/stephaneyfx/enum-iterator")
    (synopsis "Procedural macro to derive Sequence")
    (description "This package provides Procedural macro to derive Sequence.")
    (license license:bsd-0)))

(define-public rust-enum-iterator-1
  (package
    (name "rust-enum-iterator")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "enum-iterator" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zjrbmlkkrf5f0lcr61wl1zbd8iywpj29mcwsxclhrn7y5y9wvbh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-enum-iterator-derive" ,rust-enum-iterator-derive-1))))
    (home-page "https://github.com/stephaneyfx/enum-iterator")
    (synopsis
     "Tools to iterate over all values of a type (e.g. all variants of an enumeration)")
    (description
     "This package provides tools to iterate over all values of a type (e.g. all
variants of an enumeration).")
    (license license:bsd-0)))

(define-public rust-regex-syntax-0.6
  (package
    (name "rust-regex-syntax")
    (version "0.6.29")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regex-syntax" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rust-lang/regex/tree/master/regex-syntax")
    (synopsis "regular expression parser.")
    (description "This package provides a regular expression parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-regex-1
  (package
    (name "rust-regex")
    (version "1.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regex" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07cxfkk935hcjmpa9wfjh1s0ddkh9lj0ivwk90pr3b7n4hxnj7wb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-aho-corasick" ,rust-aho-corasick-0.7)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-regex-syntax" ,rust-regex-syntax-0.6))
       #:cargo-development-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis
     "An implementation of regular expressions for Rust. This implementation uses
finite automata and guarantees linear time matching on all inputs.")
    (description
     "This package provides An implementation of regular expressions for Rust.  This implementation uses
finite automata and guarantees linear time matching on all inputs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lazy-static-1
  (package
    (name "rust-lazy-static")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lazy_static" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0in6ikhw8mgl33wjv6q6xfrb5b9jr16q8ygjy803fay4zcisvaz2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-spin" ,rust-spin-0.5))
       #:cargo-development-inputs (("rust-doc-comment" ,rust-doc-comment-0.3))))
    (home-page "https://github.com/rust-lang-nursery/lazy-static.rs")
    (synopsis "macro for declaring lazily evaluated statics in Rust.")
    (description
     "This package provides a macro for declaring lazily evaluated statics in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-parking-lot-0.12
  (package
    (name "rust-parking-lot")
    (version "0.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "parking_lot" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09ws9g6245iiq8z975h8ycf818a66q3c6zv4b5h8skpm7hc1igzi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lock-api" ,rust-lock-api-0.4)
                       ("rust-parking-lot-core" ,rust-parking-lot-core-0.9))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "More compact and efficient implementations of the standard synchronization primitives")
    (description
     "This package provides More compact and efficient implementations of the standard synchronization
primitives.")
    (license (list license:expat license:asl2.0))))

(define-public rust-multiversion-0.7
  (package
    (name "rust-multiversion")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "multiversion" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xz8yy00jcpr22zc4m7azafvbdia3p88cc2pwlss4715wbnpxa76"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-multiversion-macros" ,rust-multiversion-macros-0.7)
                       ("rust-target-features" ,rust-target-features-0.1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.4)
                                   ("rust-rustversion" ,rust-rustversion-1))))
    (home-page "https://github.com/calebzulawski/multiversion")
    (synopsis "Easy function multiversioning")
    (description "This package provides Easy function multiversioning.")
    (license (list license:expat license:asl2.0))))

(define-public rust-strsim-0.10
  (package
    (name "rust-strsim")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "strsim" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rapidfuzz/strsim-rs")
    (synopsis
     "Implementations of string similarity metrics. Includes Hamming, Levenshtein,
OSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and SÃ¸rensen-Dice.")
    (description
     "This package provides Implementations of string similarity metrics.  Includes Hamming, Levenshtein,
OSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and SÃ¸rensen-Dice.")
    (license license:expat)))

(define-public rust-thiserror-impl-1
  (package
    (name "rust-thiserror-impl")
    (version "1.0.56")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror-impl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w9ldp8fa574ilz4dn7y7scpcq66vdjy59qal8qdpwsh7faal3zs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "Implementation detail of the `thiserror` crate")
    (description
     "This package provides Implementation detail of the `thiserror` crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-1
  (package
    (name "rust-thiserror")
    (version "1.0.56")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1b9hnzngjan4d89zjs16i01bcpcnvdwklyh73lj16xk28p37hhym"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-thiserror-impl" ,rust-thiserror-impl-1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-ref-cast" ,rust-ref-cast-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "derive(Error)")
    (description "This package provides derive(Error).")
    (license (list license:expat license:asl2.0))))

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
    (arguments
     `(#:cargo-inputs (("reqwest" ,rust-reqwest-0.12)
                       ("ringbuf" ,rust-ringbuf-0.3)
                       ("serde" ,rust-serde-1)
                       ("serde_json" ,rust-serde-json-1)
                       ("ron" ,rust-ron-0.8)
                       ("crossbeam" ,rust-crossbeam-0.8)
                       ("songbird" ,rust-songbird-0.3)
                       ("serenity" ,rust-serenity-0.11)
                       ("tokio" ,rust-tokio-1)
                       ("libpulse-binding" ,rust-libpulse-binding-2)
                       ("tui" ,rust-tui-0.19)
                       ("tui-textarea" ,rust-tui-textarea-0.2)
                       ("crossterm" ,rust-crossterm-0.25)
                       ("enum-iterator" ,rust-enum-iterator-1)
                       ("regex" ,rust-regex-1)
                       ("lazy_static" ,rust-lazy-static-1)
                       ("parking_lot" ,rust-parking-lot-0.12)
                       ("multiversion" ,rust-multiversion-0.7)
                       ("strsim" ,rust-strsim-0.10)
                       ("thiserror" ,rust-thiserror-1))))
    (native-inputs (list pkg-config cmake))
    (inputs (list pulseaudio opus openssl))
    (home-page "https://githubcom/Enitoni/pulseshitter/tree/main")
    (synopsis "Shit audio into a Discord bot from Pulse or Pipewire")
    (description synopsis)
    (license (list license:mpl2.0))))
pulseshitter
