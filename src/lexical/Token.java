package lexical;

import utils.TokenType;

public class Token {
	TokenType type;
	private String content;
	private int line;
	private int column;
	
	// private Token() {}

	public Token(TokenType type, String content, int line, int column) {
		super();
		this.type = type;
		this.content = content;
		this.line = line;
		this.column = column;
		System.out.println(content + " line: " + line + " column: " + column);
	}

	public int getLine() {
		return this.line;
	}

	public int getColumn() {
		return this.column;
	}

	public TokenType getType() {
		return type;
	}

	public void setType(TokenType type) {
		this.type = type;
	}

	public String getContent() {
		return content;
	}

	public void setContent(String content) {
		this.content = content;
	}

	@Override
	public String toString() {
		return "Token [type=" + type + ", content='" + content + "']";
	}
	
	
}
