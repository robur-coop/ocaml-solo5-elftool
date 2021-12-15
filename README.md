## Solo5-elftool - OCaml Solo5 elftool for querying solo5 manifests

Solo5 embeds a manifest of which devices a unikernel expects.
Solo5-elftool can be used to read and inspect this manifest.

One advantage over calling out to Solo5's `solo5-elftool` is that a user of the library can read the manifest from an ELF executable held in memory; no need to write the executable to disk first!


