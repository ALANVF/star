package compiler;

enum Body {
	BBlock(b: Block);
	BPure;
	BDefault;
	BDisable;
}