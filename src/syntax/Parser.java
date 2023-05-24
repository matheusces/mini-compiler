package syntax;

import exceptions.SyntaxException;
import lexical.Scanner;
import lexical.Token;
import utils.Keyword;
import utils.TokenType;

public class Parser {
	private Scanner scanner;
	private Token token;

	public Parser(Scanner scanner) {
		this.scanner = scanner;
	}

	public void match(Token token, TokenType type) {
		if (this.token != null) {
			if (token.getType() != type) {
				throw new SyntaxException("Type " + type + " expected, found " + token.getType());
			}
		} else {
			throw new SyntaxException("Type " + type + " expected, found null ");
		}

	}

	public void match(Token token, Keyword keyword) {
		if(this.token != null) {
			if (token.getContent().intern() != keyword.toString().intern()) {
				throw new SyntaxException("Keyword " + keyword + " expected, found " + token.getContent());
			}
		} else {
			throw new SyntaxException("Keyword " + keyword + " expected, found null");
		}

	}

	public void Programa() {
		this.token = this.scanner.nextToken();
		match(this.token, TokenType.TWO_POINTS);
		this.token = this.scanner.nextToken();
		match(this.token, Keyword.DECLARACOES);
		listaDeclaracoes();	
		this.token = this.scanner.nextToken();
		match(this.token, TokenType.DELIMITER);
		this.token = this.scanner.nextToken();
		match(this.token, TokenType.TWO_POINTS);
		this.token = this.scanner.nextToken();
		match(this.token, Keyword.ALGORITMO);
		listaComandos(null);
		this.token = this.scanner.nextToken();
		match(this.token, TokenType.DELIMITER);
	}

	public void listaDeclaracoes() {

	}

	public void listaComandos(Token token) {
		comando(token);
		listaComandos2();
		match(this.token, TokenType.DELIMITER);
	}

	public void listaComandos2() {
		this.token = this.scanner.nextToken();
		if (this.token != null && this.token.getType() != TokenType.DELIMITER) {
			listaComandos(this.token);
		} 
	}

	public void comando(Token tokenProp) {
		if (tokenProp == null) {
			this.token = this.scanner.nextToken();
		} else {
			this.token = tokenProp;
		}


        if (token.getContent().intern() == Keyword.ASSIGN.toString().intern()) {
            comandoAtribuicao();
        } else if (token.getContent().intern() == Keyword.INPUT.toString().intern()) {
            comandoEntrada();
        } else if (token.getContent().intern() == Keyword.PRINT.toString().intern()) {
            comandoSaida();
        } else if (token.getContent().intern() == Keyword.IF.toString().intern()) {
            comandoCondicao();
        } else if (token.getContent().intern() == Keyword.WHILE.toString().intern()) {
            comandoRepeticao();
        } else {
            throw new SyntaxException("Expected ASSING, INPUT, PRINT, IF or WHILE and found " + token.getContent());
        }
	}

	public void comandoAtribuicao() {
		expressaoAritmetica();
		this.token = this.scanner.nextToken();
		match(this.token, Keyword.TO);
		this.token = this.scanner.nextToken();
		match(this.token, TokenType.IDENTYFIER);
		this.token = this.scanner.nextToken();
		match(this.token, TokenType.DELIMITER);
	}

	public void comandoEntrada() {
		this.token = this.scanner.nextToken();
		match(this.token, TokenType.IDENTYFIER);
	}

	public void comandoSaida() {
		this.token = this.scanner.nextToken();
		if (token.getType() != TokenType.IDENTYFIER && token.getType() != TokenType.STRING) {
			throw new SyntaxException("Identyfier or String expected, found " + token.getType());
		}
	}
	
	public void comandoCondicao() {
		this.token = this.scanner.nextToken();
		match(this.token, Keyword.IF);
		expressaoRelacional();
		this.token = this.scanner.nextToken();
		match(this.token, Keyword.THEN);
		comando(null);
		comandoCondicao2();
		this.token = this.scanner.nextToken();
		match(this.token, TokenType.DELIMITER);
	}

	public void comandoCondicao2() {
		this.token = this.scanner.nextToken();
		if (token != null && token.getType() != TokenType.DELIMITER) {
			this.token = this.scanner.nextToken();
			match(this.token, Keyword.ELSE);
			comando(null);
		}
	}

	public void comandoRepeticao() {
		expressaoRelacional();
		comando(null);
	}

	public void expressaoAritmetica() {

	}

	public void expressaoRelacional() {

	}



	// public void E() {
	// 	T();
	// 	El();
	// }

	// private void El() {
	// 	this.token = this.scanner.nextToken();
	// 	if (this.token != null) {
	// 		OP();
	// 		T();
	// 		El();
	// 	}
	// }

	// private void OP() {
	// 	if (this.token.getType() != TokenType.MATH_OP && this.token.getType() != TokenType.REL_OP) {
	// 		throw new SyntaxException("Math or Relational operator expected, found " + token.getType());
	// 	}
	// }

	// private void T() {
	// 	this.token = this.scanner.nextToken();
	// 	if (this.token.getType() != TokenType.IDENTYFIER && this.token.getType() != TokenType.NUMBER) {
	// 		throw new SyntaxException("Identyfier or Number expected, found " + token.getType());
	// 	}

	// }

}
