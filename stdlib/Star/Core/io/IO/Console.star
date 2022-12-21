module Console {
	my stdin (In) is getter
	my stdout (Out) is getter
	my stderr (Out) is getter

	on [getStdin] (Handle) is hidden is native `io_stdin`
	on [getStdout] (Handle) is hidden is native `io_stdout`
	on [getStderr] (Handle) is hidden is native `io_stderr`

	init {
		stdin = In[fromHandle: This[getStdin]]
		stdout = Out[fromHandle: This[getStdout]]
		stderr = Out[fromHandle: This[getStderr]]
	}
}