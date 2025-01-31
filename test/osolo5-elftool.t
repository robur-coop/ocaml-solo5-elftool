Execute osolo5-elftool on hvt binary

  $ osolo5-elftool query-abi test_hello.hvt
  { "type": "solo5.abi", "target": "hvt", "version": 2 
  }
  $ osolo5-elftool query-manifest test_hello.hvt
  { "type": "solo5.manifest", "version": 1, "devices": [  ]
  }
  $ osolo5-elftool query-abi test_hello.muen
  { "type": "solo5.abi", "target": "muen", "version": 3 
  }
  $ osolo5-elftool query-abi test_hello.spt
  { "type": "solo5.abi", "target": "spt", "version": 2 
  }
  $ osolo5-elftool query-abi test_hello.virtio
  { "type": "solo5.abi", "target": "virtio", "version": 1 
  }
  $ osolo5-elftool query-abi test_hello.xen
  { "type": "solo5.abi", "target": "xen", "version": 1 
  }
