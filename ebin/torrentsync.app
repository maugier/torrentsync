{application, torrentsync,
	[
		{description, "TorrentSync"},
		{vsn, "1"},
		{modules, [
			benc,
			checker,
			differ,
			loader,
			pad_read,
			store,
			torrent,
			tsync
		]},
		{applications, [kernel, stdlib, crypto]}
	]
}.
