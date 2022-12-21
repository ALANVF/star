module Core {
	type T
	on [say: value (T)] is native `debug_print` ;@@ TEMP

	on [prompt: (Str)] (Str) {
		IO.Console.stdout[IO[Str] write: prompt]
		return IO.Console.stdin[IO[Str] readLine]
	}
}