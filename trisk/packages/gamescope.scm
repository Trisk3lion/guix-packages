(define-module (trisk packages gamescope)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages benchmark)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-26))

;; In anticipation for this to be included in Guix proper: https://issues.guix.gnu.org/70493

;; Upstream strongly recommends using some of its pinned dependencies due to
;; relying on unstable features; these should be checked when updating
;; gamescope.  See:
;; <https://github.com/ValveSoftware/gamescope/commit/7741cd587fa2274989f3307a3c6f23ab08e98460>
(define %gamescope-version "3.16.4")

(define libliftoff-for-gamescope
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.freedesktop.org/emersion/libliftoff.git")
          (commit "8b08dc1c14fd019cc90ddabe34ad16596b0691f4")))
    (file-name (git-file-name "libliftoff-for-gamescope" %gamescope-version))
    (sha256 (base32 "163g8ndsbma7acy2k9mrnvlpb7yi4431hgkx1gygkafgwpq1ii1x"))))

(define reshade-for-gamescope
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/Joshua-Ashton/reshade")
          (commit "696b14cd6006ae9ca174e6164450619ace043283")))
    (file-name (git-file-name "reshade-for-gamescope" %gamescope-version))
    (sha256
     (base32 "1zvhf3pgd8bhn8bynrsh725xn1dszsf05j8c9g6zabgv7vnz04a5"))))

(define vkroots-for-gamescope
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/Joshua-Ashton/vkroots")
          (commit "5106d8a0df95de66cc58dc1ea37e69c99afc9540")))
    (file-name (git-file-name "vkroots-for-gamescope" %gamescope-version))
    (sha256 (base32 "0hrp0xqq93552ipw2bmryixgm1aywnz49xagsx5rwzg2d0hwa0aa"))))

(define wlroots-for-gamescope
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/Joshua-Ashton/wlroots.git")
          (commit "4bc5333a2cbba0b0b88559f281dbde04b849e6ef")))
    (file-name (git-file-name "wlroots-for-gamescope" %gamescope-version))
    (sha256 (base32 "14m9j9qkaphzm3g36im43b6h92rh3xyjh7j46vw9w2qm602ndwcf"))))

(define-public gamescope
  (package
    (name "gamescope")
    (version %gamescope-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ValveSoftware/gamescope")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09h7046vwqn0w3kv1zaij4h3rcrvs1r2qlm0vva3mk3gg44fnhjl"))
       (modules '((guix build utils)
                  (ice-9 match)))
       (snippet
        #~(begin
            ;; Add some dependencies to source tree where they're expected.
            (for-each (match-lambda
                        ((source dest)
                         (copy-recursively source dest)))
                      '((#$libliftoff-for-gamescope "subprojects/libliftoff")
                        (#$reshade-for-gamescope "src/reshade")
                        (#$vkroots-for-gamescope "subprojects/vkroots")
                        (#$wlroots-for-gamescope "subprojects/wlroots")))))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-Dpipewire=enabled"
                                "-Denable_openvr_support=false")
      #:modules '((guix build meson-build-system)
                  (guix build utils)
                  (srfi srfi-26))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-usr-dir
            (lambda _
              (substitute* "src/reshade_effect_manager.cpp"
                (("/usr") #$output))))
          (add-after 'unpack 'patch-loader-path
            ;; "Failed to load vulkan module" error occurs without this patch.
            ;; Related issue: https://issues.guix.gnu.org/71109
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/rendervulkan.cpp"
                (("dlopen\\( \"libvulkan\\.so")
                 (string-append "dlopen( \""
                                (search-input-file
                                 inputs "/lib/libvulkan.so"))))))
          (add-after 'unpack 'patch-version
            (lambda _
              (substitute* "src/meson.build"
                (("^vcs_tag = .*$")
                 (string-append
                  "vcs_tag = '" #$(package-version this-package) "'\n")))))
          (add-after 'unpack 'patch-stb
            (lambda _
              (let ((stb-files-dir #+(directory-union
                                      "stb"
                                      (map (cut this-package-native-input <>)
                                           (list "stb-image"
                                                 "stb-image-write"
                                                 "stb-image-resize2")))))
                (copy-recursively (string-append stb-files-dir "/include")
                                  "subprojects/stb"))
              (copy-recursively "subprojects/packagefiles/stb"
                                "subprojects/stb")
              (call-with-output-file "subprojects/stb.wrap"
                (cut format <> "\
[wrap-file]
directory = stb
"))))
          (add-after 'unpack 'patch-spirv-headers
            (lambda _
              (substitute* "src/meson.build"
                (("../thirdparty/SPIRV-Headers")
                 #$(this-package-native-input "spirv-headers"))))))))
    (native-inputs (list gcc-14
                         glslang
                         pkg-config
                         python-3
                         spirv-headers
                         stb-image
                         stb-image-write
                         stb-image-resize2
                         vulkan-headers))
    (inputs (list benchmark
                  glm
                  hwdata
                  lcms
                  libavif
                  libdecor
                  libdisplay-info
                  libdrm
                  libei
                  libinput
                  libseat
                  libx11
                  libxcomposite
                  libxcursor
                  libxdamage
                  libxext
                  libxkbcommon
                  libxmu
                  libxrender
                  libxres
                  libxt
                  libxtst
                  libxxf86vm
                  luajit
                  pipewire
                  pixman
                  sdl2
                  vulkan-loader
                  xcb-util-wm
                  xcb-util-errors
                  xorg-server-xwayland
                  wayland
                  wayland-protocols))
    (home-page "https://github.com/ValveSoftware/gamescope")
    (synopsis "Micro-compositor for running games")
    (description
     "gamescope is a micro-compositor for running games.  Its goal is to
provide an isolated compositor that is tailored towards gaming and supports
many gaming-centric features such as:
@itemize
@item Spoofing resolutions.
@item Upscaling.
@item Limiting framerates.
@end itemize")
    (license license:bsd-2)))
