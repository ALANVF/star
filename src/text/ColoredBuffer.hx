package text;

import haxe.io.BytesData;
import hx.strings.Char;
import hx.strings.ansi.*;
import hx.strings.StringBuilder;

@:structInit
@:publicFields
class Color {
	final fg: Option<AnsiColor>;
	final bg: Option<AnsiColor>;
	
	function equals(color: Color) {
		return this == color || (fg.equals(color.fg) && bg.equals(color.bg));
	}
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
		ensureBuffer(left + str.length8(), top);
		
		final line = lines[top];
		
		for(i in 0...str.length8()) {
			final c = str.charCodeAt8(i);
			if(c==-1) throw "NUUUU";
			line.text.setChar(left + i, c);
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

				if(line.text.length < xp) {
					for(_ in 0...left+2) line.color.push({fg: fg, bg: bg});
					line.text.add(" ".repeat(left));
					line.text.add(ch.toString());
					line.text.add(" ".repeat(xp - line.text.length));
					line.text = new StringBuilder(line.text.toString().trimRight());
				} else {
					line.text.setChar(xp, ch);
					line.color[xp] = {fg: fg, bg: bg};
				}
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
		// DO NOT TOUCH ANY OF THE WINDOWS STUFF I HATE IT THX

		writer.write("\033[0m");
		#if windows
			writer.cursor(SavePos);
			while(true) try {
				Sys.sleep(0.01);
				writer.write(" ");
				writer.cursor(RestorePos);
				break;
			} catch(_: haxe.io.Eof) {
				Sys.sleep(0.01);
			}
		#end
		for(line in lines) {
			#if windows
				writer.write("\033[0m");
				writer.cursor(SavePos);
				writer.write("\033[0m");
				writer.write(" ".repeat(line.text.length) + " \033[0m");
				writer.write("\033[G\r");
				writer.cursor(RestorePos);
				writer.write("\033[0m\r");
			#end

			final lineStr: String = untyped line.text.toString();
			final lineLen = lineStr.length8();
			var i = 0;

			while(i < lineLen) {
				{
					final c = line.color[i];
					if(c == null) throw '??? $i $lineLen';
					switch c.fg {
						case Some(f):
							#if windows
								hl.Gc.blocking(true);
								// Buffer the terminal because it's stupid
								writer.write("\033[39m\033[0m");
								writer.write("\033[39m\033[0m");
								writer.write("\033[39m\033[0m");
								writer.write("\033[39m\033[0m");
								writer.write("\033[39m\033[0m");
								#end
							writer.fg(f);
							#if windows
								hl.Gc.blocking(false);
							#end
						
						case None:
							#if windows
								hl.Gc.blocking(true);
								// Buffer the terminal because it's stupid
								writer.write("\033[39m\033[0m");
								writer.write("\033[39m\033[0m");
								writer.write("\033[39m\033[0m");
								writer.write("\033[39m\033[0m");
								writer.write("\033[39m\033[0m");
							#end
							writer.write("\033[39m\033[0m");
							#if windows
								hl.Gc.blocking(false);
							#end
					}
				}

				final start = i++;
				
				while(i < lineLen && line.color[start].equals(line.color[i])) i++;

				// Print portion
				final sub = lineStr.substring8(start, i);
				#if windows
					// Windows terminal doesn't like unicode strings for some reason :/
					for(ch in sub.toChars()) {
						try {
							writer.cursor(SavePos);
							final b = ch.toString().toBytes();
							writer.out.writeFullBytes(b, 0, b.length);
						} catch(_: haxe.io.Eof) {
							if(ch <= 0xff) {
								writer.cursor(RestorePos);
								writer.out.writeByte(ch.toInt());
							} else
								throw 'bad ${ch.toInt()} $sub';
						}
					}
				#else
					writer.write(sub);
				#end
				
			}
			
			#if windows
				writer.write("\033[0m");
				try {
					writer.cursor(SavePos);
					writer.write("\n\033[0m");
				} catch(_: haxe.io.Eof) {
					while(true) try {
						hl.Gc.blocking(true);
						writer.cursor(RestorePos);
						writer.write("\033[0m ");
						writer.write("\n\033[0m");
						hl.Gc.blocking(false);
						break;
					} catch(_: haxe.io.Eof) {
						Sys.sleep(0.01);
						writer.write("");
						continue;
					}
				}
				writer.write("\033[0m");
			#else
				writer.write("\033[0m\n");
			#end
		}
		
		#if windows
			writer.write("\033[0m");
			try {
				writer.write("\n\033[0m");
			} catch(_: haxe.io.Eof) {
				writer.write("\n\033[0m");
			}
			writer.write("\033[0m");
		#else
			writer.write("\033[0m\n");
		#end
		writer.flush();
	}

	private function ensureBuffer(x: Int, y: Int) {
		// First we ensure y exists
		while(lines.length <= y) lines.push({});

		// Now ensure x character in line y
		final line = lines[y];
		final requiredChars = (x - line.text.length) + 1;
		
		if(requiredChars > 0) {
			line.text.add(" ".repeat(requiredChars));
			for(_ in 0...requiredChars) line.color.push({fg: fg, bg: bg});
		}
	}
}