package text;

import hx.strings.Char;
import hx.strings.ansi.*;
import hx.strings.StringBuilder;

@:struct
@:structInit
@:publicFields
class Color {
	final fg: Option<AnsiColor>;
	final bg: Option<AnsiColor>;
}

@:publicFields
@:structInit
private class Line {
	var text: StringBuilder = new StringBuilder();
	var color: Array<Color> = [];
}

@:publicFields
class ColoredBuffer {
	var fg: Option<AnsiColor> = None;
	var bg: Option<AnsiColor> = None;
	var cursorX: Int = 0;
	var cursorY: Int = 0;
	private var lines: Array<Line> = [];

	function new() {}

	function clear() {
		cursorX = 0;
		cursorY = 0;
		resetColor();
		lines = [];
	}

	function resetColor() {
		fg = None;
		bg = None;
	}

	function plot(x: Int, y: Int, ch: Char) {
		ensureBuffer(x, y);
		
		final line = lines[y];
		
		line.text.setChar(x, ch);
		line.color[x] = {fg: fg, bg: bg};
		
		cursorX = x + 1;
		cursorY = y;
	}

	function writeAt(left: Int, top: Int, str: String) {
		ensureBuffer(left + str.length8() - 1, top);
		
		final line = lines[top];
		
		for(i in 0...str.length8()) {
			line.text.setChar(left + i, str.charCodeAt8(i));
			line.color[left + i] = {fg: fg, bg: bg};
		}
		
		cursorX = left + str.length8();
		cursorY = top;
	}

	function writeChar(ch: Char) plot(cursorX, cursorY, ch);
	function write(str: String) writeAt(cursorX, cursorY, str);

	function newLine() {
		cursorX = 0;
		cursorY++;
	}

	function writeLine(str: String) {
		write(str);
		newLine();
	}

	function fill(left: Int, top: Int, width: Int, height: Int, ch: Char) {
		for(j in 0...height) {
			final yp = top + j;
			
			ensureBuffer(left + width - 1, yp);
			
			final line = lines[yp];
			
			for(i in 0...width) {
				final xp = left + i;

				line.text.setChar(xp, ch);
				line.color[xp] = {fg: fg, bg: bg};
			}
		}

		cursorX = left + width;
		cursorY = top + height - 1;
	}

	function recolor(x: Int, y: Int) {
		ensureBuffer(x, y);
		var line = lines[y];
		line.color[x] = {fg: fg, bg: bg};
	}

	function recolorArea(left: Int, top: Int, width: Int, height: Int) {
		for(j in 0...height) {
			final yp = top + j;

			ensureBuffer(left + width - 1, yp);
			
			final line = lines[yp];
			
			for(i in 0...width) {
				final xp = left + i;
				
				line.color[xp] = {fg: fg, bg: bg};
			}
		}
	}

	function outputTo(writer: AnsiWriter<haxe.io.Output>) {
		var lastColor: Color = {fg: None, bg: None};

		for(line in lines) {
			final lineStr = line.text.toString();
			var i = 0;

			while(i < line.text.length) {
				final start = i;

				while(i < line.text.length && line.color[i] == lastColor) i++;
				
				// Print portion
				writer.write(lineStr.substr8(start, i - start));
				
				// If the line has not ended, we must have changed color
				if(i < line.text.length) {
					final c = line.color[i];

					switch [c.fg, c.bg] {
						case [Some(fg), Some(bg)]:
							writer.fg(fg);
							writer.bg(bg);
						
						case [Some(fg), None]:
							writer.attr(RESET);
							writer.fg(fg);
						
						case [None, Some(bg)]:
							writer.attr(RESET);
							writer.bg(bg);

						case [None, None]:
							writer.attr(RESET);
					}
					
					lastColor = c;
				}
			}
			writer.write(Strings.NEW_LINE);
		}

		writer.flush();
		writer.attr(RESET);
	}

	private function ensureBuffer(x: Int, y: Int) {
		// First we ensure y exists
		while(lines.length <= y) lines.push({});

		// Now ensure x character in line y
		final line = lines[y];
		final requiredChars = x - line.text.length + 1;
		
		if(requiredChars > 0) {
			line.text.add(" ".repeat(requiredChars));
			for(_ in 0...requiredChars) line.color.push({fg: fg, bg: bg});
		}
	}
}