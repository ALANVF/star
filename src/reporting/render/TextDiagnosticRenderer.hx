package reporting.render;

import text.ColoredBuffer;
import hx.strings.ansi.AnsiColor;
import hx.strings.Char;
//import hx.strings.Strings;
import text.LineCursor;
import text.SourceFile;
//import hx.strings.StringBuilder;
import hx.strings.ansi.Ansi;
import hx.strings.ansi.AnsiWriter;
import reporting.DiagnosticInfo;

using util.Strings;
private enum Line {
	Source(source: SourceFile, line: Int);
	Dot;
	Annotation(line: Int, annotations: Array<SpannedInfo>);
}

@:publicFields
class TextDiagnosticRenderer implements IDiagnosticRenderer {
	var writer: AnsiWriter<haxe.io.Output>;
	var surroundingLines = 1;
	var connectUpLines = 1;
	var tabSize = 4;
	var syntaxHighlighter: ISyntaxHighlighter = new NullSyntaxHighlighter();
	private var buffer: ColoredBuffer;

	function new(?writer: haxe.io.Output) {
		this.writer = Ansi.writer(writer ?? Sys.stderr());
		buffer = new ColoredBuffer();
	}

	function render(diagnostic: Diagnostic) {
		buffer = new ColoredBuffer();

		renderDiagnosticHead(diagnostic);

		final spannedInfo = diagnostic.info
			.filterMap(i -> i.getSpannedInfo())
			.sorted((l, r) -> l.span.start.compare(r.span.start))
			.groupBy(i -> i.span.source.nonNull(), (l, r) -> l.fullPath == r.fullPath);
		
		for(infos in spannedInfo) renderSpannedGroup(infos);

		diagnostic.info.filterMap(i -> i.getFootnote()).forEach(renderFootnote);

		buffer.outputTo(writer);
		writer.write("\033[0m");
	}

	private function renderDiagnosticHead(diagnostic: Diagnostic) {
		switch diagnostic {
			case {severity: null, message: null}:
			default:
				diagnostic.severity._and(s => {
					buffer.fg = Some(s.color);
					buffer.write(s.description);
					
					diagnostic.code._and(c => buffer.write('[$c]'));

					buffer.resetColor();

					if(diagnostic.message != null) {
						buffer.write(": ");
					}
				});

				buffer.resetColor();
				diagnostic.message._and(m => buffer.write(m));

				buffer.newLine();
		}
	}

	private function renderSpannedGroup(infos: Array<SpannedInfo>) {
		final sourceFile = infos[0].span.source.nonNull();
		final linePrimitives = collectLinesToRender(infos);
		final maxLineIndex = linePrimitives.filterMap(l -> switch l {
			case Source(_, line): line;
			default: null;
		}).max();
		final lineNumberPadding = " ".repeat(Std.string(maxLineIndex + 1).length);

		buffer.write('$lineNumberPadding ┌─ ${sourceFile.path}');
		
		switch infos.find(i -> i.isPrimary) {
			case null:
			case {span: span}: buffer.write(':${span.start.line + 1}:${span.start.column}');
		}
		
		buffer.newLine();
		buffer.writeLine('$lineNumberPadding │');
		
		for(line in linePrimitives) {
			switch line {
				case Source(source, line):
					buffer.write('${Std.string(line + 1).lpad(lineNumberPadding.length)} │ ');
					renderSourceLine(source, line);
					buffer.newLine();

				case Annotation(line, annotations):
					renderAnnotationLines(line, annotations, '$lineNumberPadding │ ');

				case Dot:
					buffer.writeLine('$lineNumberPadding │ ...');
			}
		}
		
		buffer.writeLine('$lineNumberPadding │');
	}
	
	private function renderSourceLine(source: SourceFile, line: Int) {
		final xOffset = buffer.cursorX;
		
		buffer.fg = Some(WHITE);
		
		final sourceLine = source.line(line);
		final lineCur: LineCursor = {tabSize: tabSize};

		for(ch in sourceLine.iterator()) {
			if(ch == Char.CR || ch == Char.LF) break;
			switch lineCur.append(ch) {
				case {isTab: true, advance: advance}: buffer.cursorX += advance;
				default: buffer.write(ch);
			}
		}
		
		final tokenInfo = syntaxHighlighter.highlightLine(source, line);

		if(tokenInfo.length > 0) {
			var charIdx = 0;
			
			lineCur.column = 0;
			
			for(token in tokenInfo.sorted((l, r) -> l.startIndex - r.startIndex)) {
				while(charIdx < token.startIndex) {
					lineCur.append(sourceLine.charCodeAt8(charIdx));
					charIdx++;
				}
				
				final tokenStart = lineCur.column;

				while(charIdx < token.startIndex + token.length) {
					lineCur.append(sourceLine.charCodeAt8(charIdx));
					charIdx++;
				}
				
				final tokenEnd = lineCur.column;
				
				buffer.fg = Some(tokenKindToColor(token.kind));
				buffer.recolorArea(xOffset + tokenStart, buffer.cursorY, tokenEnd - tokenStart, 1);
			}
		}

		buffer.resetColor();
	}

	private function renderAnnotationLines(line: Int, annotations: Array<SpannedInfo>, prefix: String) {
		final sourceFile = annotations[0].span.source.nonNull();
		final line = sourceFile.line(line).trimRight();
		final annotationsOrdered = annotations.sorted((l, r) -> l.span.start.compare(r.span.start));
		final arrowHeadColumns: Array<{column: Int, info: SpannedInfo}> = [];
		
		buffer.write(prefix);
		
		final lineCur: LineCursor = {tabSize: tabSize};
		var charIdx = 0;

		for(annot in annotationsOrdered) {
			while(charIdx < annot.span.start.column) {
				if(charIdx < line.length) {
					buffer.cursorX += lineCur.append(line.charAt8(charIdx)).advance;
				} else {
					buffer.cursorX++;
				}

				charIdx++;
			}
			
			final arrowHead =
				if(annot.isPrimary || annot.isSecondary) '⌃'
				else '-';
			final startColumn = buffer.cursorX;
			
			arrowHeadColumns.push({column: startColumn, info: annot});
			
			if(annot.isPrimary) buffer.fg = Some(RED);
			else if(annot.isSecondary) buffer.fg = Some(YELLOW);
			
			if((annot.isPrimary || annot.isSecondary) && annot.message != null) {
				buffer.write('↑');
				charIdx++;
			}

			while(charIdx < annot.span.end.column) {
				buffer.write(
					if(charIdx < line.length) {
						arrowHead.repeat(lineCur.append(line.charCodeAt8(charIdx)).advance);
					} else {
						arrowHead;
					}
				);

				charIdx++;
			}

			if(annot.isPrimary || annot.isSecondary) {
				// recolor line
				buffer.recolorArea(startColumn, buffer.cursorY - 1, buffer.cursorX - startColumn, 1);
				buffer.resetColor();
			}
		}
		
		/*switch annotationsOrdered.last().message {
			case null:
			case msg: buffer.write(' $msg');
		}*/
		buffer.newLine();
		
		// From now on all previous ones will be one longer than the ones later
		var arrowBaseLine = buffer.cursorY;
		var arrowBodyLength = 0;

		// We only consider annotations with messages
		for(i => v in arrowHeadColumns.reversed()/*.slice(1)*/.filter(a -> a.info.message != null)) {
			final col = v.column;
			final annot = v.info;

			if(annot.isPrimary) buffer.fg = Some(RED);
			else if(annot.isSecondary) buffer.fg = Some(YELLOW);

			// Draw the arrow
			buffer.fill(col, arrowBaseLine, 1, arrowBodyLength + i, '│');
			buffer.plot(col, arrowBaseLine + arrowBodyLength + i, '╰');
			arrowBodyLength++;
			arrowBodyLength += i;
			
			// Append the message
			if(annot.message.contains("\n")) {
				final msgLines = annot.message.split8("\n");
				final oldX = buffer.cursorX;
				buffer.write(' ${msgLines.shift()}');
				for(j => msgLine in msgLines) {
					buffer.cursorX = oldX;
					buffer.cursorY++;
					arrowBaseLine++;
					buffer.write(' $msgLine');
				}
			} else {
				buffer.write(' ${annot.message}');

			}
			if(annot.isPrimary || annot.isSecondary) buffer.resetColor();
		}
		
		// Fill the in between lines with the prefix
		for(i in 0...arrowBodyLength) {
			buffer.writeAt(0, arrowBaseLine + i, prefix);
		}

		// Reset cursor position
		buffer.cursorX = 0;
		buffer.cursorY = arrowBaseLine + arrowBodyLength;
	}

	// Collects all the line subgroups
	private function collectLinesToRender(infos: Array<SpannedInfo>): Array<Line> {
		final result = [];
		
		// We need to group the spanned informations per line
		final groupedInfos = infos.groupBy(si -> si.span.start.line).pairs().sorted((a, b) -> a.key - b.key);
		final sourceFile = infos[0].span.source.nonNull();

		// Now we collect each line primitive
		var lastLineIndex: Null<Int> = null;

		for(j in 0...groupedInfos.length) {
			final infoGroup = groupedInfos[j];
			
			// First we determine the range we need to print for this info
			final currentLineIndex = infoGroup.key;
			final minLineIndex = (lastLineIndex ?? 0).max(currentLineIndex - surroundingLines);
			var maxLineIndex = sourceFile.lineCount.min(currentLineIndex + surroundingLines + 1);
			
			if(j < groupedInfos.length - 1) {
				// There's a chance we step over to the next annotation
				final nextGroupLineIndex = groupedInfos[j + 1].key;
				maxLineIndex = maxLineIndex.min(nextGroupLineIndex);
			}
			
			// Determine if we need dotting or a line in between
			lastLineIndex._and(index => {
				final difference = minLineIndex - index;
				if(difference <= connectUpLines) {
					// Difference is negligible, connect them up, no reason to dot it out
					for(i in 0...difference) {
						result.push(Source(sourceFile, index + i));
					}
				} else {
					// Bigger difference, dot out
					result.push(Dot);
				}
			});
			lastLineIndex = maxLineIndex;
			
			// Now we need to print all the relevant lines
			for(i in minLineIndex...maxLineIndex) {
				result.push(Source(sourceFile, i));
				
				// If this was an annotated line, yield the annotation
				if(i == infoGroup.key) result.push(Annotation(i, infoGroup.value));
			}
		}
		
		return result;
	}

	private function renderFootnote(hint: {message: String}) buffer.writeLine(hint.message);

	private function tokenKindToColor(tokenKind: TokenKind) return switch tokenKind {
		case TokenKind.COMMENT: AnsiColor.GREEN;//AnsiColor.DARKGREEN;
		case TokenKind.KEYWORD: AnsiColor.MAGENTA;
		case TokenKind.LITERAL: AnsiColor.BLUE;
		case TokenKind.NAME: AnsiColor.CYAN;
		case TokenKind.PUNCTUATION: AnsiColor.WHITE;
		case TokenKind.OTHER: AnsiColor.WHITE;
		case TokenKind.OPERATOR: AnsiColor.CYAN;//AnsiColor.DARKCYAN;
		default: throw new NotImplementedException();
	};
}