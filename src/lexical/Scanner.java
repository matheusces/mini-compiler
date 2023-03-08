package lexical;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

import utils.TokenType;

public class Scanner {

	int pos;
	char[] contentTXT;
	int state;

	public Scanner(String filename) {
		try {
			String contentBuffer = new String(Files.readAllBytes(Paths.get(filename)), StandardCharsets.UTF_8);
			this.contentTXT = contentBuffer.toCharArray();
			this.pos = 0;
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public Token nextToken() {
		this.state = 0;
		String content = "";
		char currentChar;

		while (true) {
			if (isEOF()) {
				return null;
			}
			currentChar = this.nextChar();

			switch (state) {
			case 0:
				if (this.isLetter(currentChar)) {
					content += currentChar;
					state = 1;
				} else if (isSpace(currentChar)) {
					state = 0;
				} else if (isDigit(currentChar)) {
					content += currentChar;
					state = 2;
				}
				break;
			case 1:
				if (this.isLetter(currentChar) || this.isDigit(currentChar)) {
					content += currentChar;
					state = 1;
				} else {
					this.back();
					return new Token(TokenType.IDENTYFIER, content);
				}
				break;
			case 2:
				if(isDigit(currentChar)) {
					content += currentChar;
					state = 2;
				} else if(isLetter(currentChar)) {
					throw new RuntimeException("Number Malformed!");
				} else {
					this.back();
					return new Token(TokenType.NUMBER, content);
				}
				break;
			}
		}
	}

	private char nextChar() {
		return this.contentTXT[this.pos++];
	}

	private void back() {
		this.pos--;
	}

	private boolean isLetter(char c) {
		return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
	}

	private boolean isDigit(char c) {
		return c >= '0' && c <= '9';
	}

	private boolean isOperator(char c) {
		return c == '>' || c == '=' || c == '<' || c == '!';
	}

	private boolean isSpace(char c) {
		return c == ' ' || c == '\n' || c == '\t' || c == '\r';
	}

	private boolean isEOF() {
		if (this.pos >= this.contentTXT.length) {
			return true;
		}
		return false;
	}

}
