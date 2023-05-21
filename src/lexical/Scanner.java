package lexical;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

import utils.TokenType;
import utils.Keyword;

public class Scanner {

	int pos;
	char[] contentTXT;
	int state;
	int line;
	int column;

	public Scanner(String filename) {
		try {
			String contentBuffer = new String(Files.readAllBytes(Paths.get(filename)), StandardCharsets.UTF_8);
			this.contentTXT = contentBuffer.toCharArray();
			this.pos = 0;
			this.line = 1;
			this.column = 1;

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

			if (isEndOfLine(currentChar)) {
				this.line += 1;
				this.column = 1;
			}

			switch (state) {
				case 0:
					if (currentChar == '#') {
						while (!this.isEndOfLine(currentChar)) {
                            currentChar = this.nextChar();
                        }
                        state = 0;
					}else if (this.isLetter(currentChar)) {
						content += currentChar;
						state = 1;
					} else if (isSpace(currentChar)) {
						state = 0;
					} else if (isDigit(currentChar)) {
						content += currentChar;
						state = 2;
					} else if (isMathOperator(currentChar)) {
						content += currentChar;
						state = 3;
					} else if (isEquals(currentChar)) {
						content += currentChar;
						state = 4;
					} else if (isLess(currentChar)) {
						content += currentChar;
						state = 5;
					} else if (isGreater(currentChar)) {
						content += currentChar;
						state = 6;
					} else if (isExclamation(currentChar)) {
						content += currentChar;
						state = 7;
					} else if(isLeftParenthesis(currentChar)) {
						content += currentChar;
           				return new Token(TokenType.LEFT_PARENTHESIS, content);
					} else if(isRightParenthesis(currentChar)) {
						content += currentChar;
            			return new Token(TokenType.RIGHT_PARENTHESIS, content);
					} else if (isDot(currentChar)) {
						content += currentChar;
						state = 8;
					} else {
						throw new RuntimeException("Error: Invalid Character [line:" + this.line  + " ] [column:"+ this.column + "]");
					}
					break;
				case 1:
					if (this.isLetter(currentChar) || this.isDigit(currentChar) || this.isUnderscore(currentChar)) {
						content += currentChar;
						state = 1;
					} else {
						if (!isEndOfLine(currentChar)) this.back();
						if (isSpace(currentChar)) {
							for (Keyword k : Keyword.values()) {
								if (content.intern() == k.toString().toLowerCase().intern()) {
									return new Token(TokenType.RESERVED_KEYWORD, k.toString());
								}
							}

							return new Token(TokenType.IDENTYFIER, content);
						}
						throw new RuntimeException("Error: Invalid character for Identifyer: [line:" + this.line  + " ] [column:"+ this.column + "]");
					}
					break;
				case 2:
					if(isDigit(currentChar)) {
						content += currentChar;
						state = 2;
					} else if(currentChar == '.') {
						content += currentChar;
						state = 8;
					} else	if (isSpace(currentChar) || isEndOfLine(currentChar) || isRightParenthesis(currentChar)) {
						return new Token(TokenType.NUMBER, content);
					} else {
						throw new RuntimeException("Error: Invalid Character for Number [line:" + this.line  + " ] [column:"+ this.column + "]");
					}
					break;
					
				case 3: 
					return getMathToken(content);
				case 4: 
					if (isEquals(currentChar) && isEquals(this.contentTXT[this.pos])) {
						this.nextChar();
						throw new RuntimeException("Error: Invalid Character [line:" + this.line  + " ] [column:"+ this.column + "]");

					} else if(isEquals(currentChar) && !isEquals(this.contentTXT[this.pos])) {
						content += currentChar;
						return new Token(TokenType.EQUALS_OP, content);
					} else {
						if (!isEndOfLine(currentChar)) this.back();
						return new Token(TokenType.ASSIGN_OP, content);
					}

				case 5:
					if (isEquals(currentChar)) {
						content += currentChar;
						return new Token(TokenType.LESS_EQUALS_OP, content);
					} else {
						if (!isEndOfLine(currentChar)) this.back();
						return new Token(TokenType.LESS_OP, content);
					}
				
				case 6:
					if (isEquals(currentChar)) {
						content += currentChar;
						return new Token(TokenType.GREATER_EQUALS_OP, content);
					} else {
						if (!isEndOfLine(currentChar)) this.back();
						return new Token(TokenType.GREATER_OP, content);
					}

				case 7:
					if (isEquals(currentChar)) {
						content += currentChar;
						return new Token(TokenType.DIF_OP, content);
					} else {
						if (!isEndOfLine(currentChar)) this.back();
						throw new RuntimeException("Operator ! don't is supported [line:" + line  + " ] [column:"+ column + "]");
					}
				case 8:
					if (isDigit(currentChar)) {
						content += currentChar;
						state = 9;
					} else {
						this.back();
						throw new RuntimeException("Error: Invalid Character for Number [line:" + line  + " ] [column:"+ column + "]");
					}
					break;

				case 9: 
					if (isDigit(currentChar)) {
						content += currentChar;
						state = 9;
					} else if (isSpace(currentChar) || isEndOfLine(currentChar)) {
						return new Token(TokenType.NUMBER, content);
					} else {
						throw new RuntimeException("Error: Invalid Character for Number [line:" + line  + " ] [column:"+ column + "]");
					}
					break;
			}
		}
	}

	private char nextChar() {
		this.column++;
		return this.contentTXT[this.pos++];
	}

	private void back() {
		this.column--;
		this.pos--;
	}

	private boolean isLetter(char c) {
		return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
	}

	private boolean isDigit(char c) {
		return c >= '0' && c <= '9';
	}

	private boolean isUnderscore(char c) {
		return c == '_';
	}

	private boolean isLess(char c) {
		return c == '<';
	}

	private boolean isGreater(char c) {
		return c == '>';
	}

	private boolean isDot(char c) {
		return c == '.';
	}

	private boolean isExclamation(char c) {
		return c == '!';
	}

	private boolean isSpace(char c) {
		return c == ' ' || c == '\n' || c == '\t' || c == '\r';
	}

	private boolean isEndOfLine(char c) {
		return c == '\n' || c == '\r';
	}

	private boolean isEquals(char c) {
		return c == '=';
	}

	private boolean isMathOperator(char c) {
		return c == '+' || c == '-' || c == '*' || c == '/';
	}

	private boolean isEOF() {
		if (this.pos >= this.contentTXT.length) {
			return true;
		}
		return false;
	}

	private boolean isEspecialCharacter(String c) {
		return c.matches("[\\p{Punct}]");
	}

	private Token getMathToken(String c) {
		this.back();
		switch (c) {
			case "+":
				return new Token(TokenType.SUM_OP, c);
			case "-":
				return new Token(TokenType.SUB_OP, c);
			case "*":
				return new Token(TokenType.MULT_OP, c);
			default: 
				return new Token(TokenType.DIV_OP, c);	
		}
	}

	private boolean isLeftParenthesis(char c) {
		return c == '(';
	}
	
	private boolean isRightParenthesis(char c) {
		return c == ')';
	}

}
