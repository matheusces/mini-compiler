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
        declaracao();
        listaDeclaracoes2();
        this.token = this.scanner.nextToken();
        match(this.token, TokenType.DELIMITER);
	}

    public void listaDeclaracoes2() {
        this.token = this.scanner.nextToken();
        if (this.token != null && this.token.getType() != TokenType.DELIMITER) {
            listaDeclaracoes();
        }
        
	}

    public void declaracao() {
        tipoVar();
        this.token = this.scanner.nextToken();
        match(this.token, TokenType.TWO_POINTS);
        this.token = this.scanner.nextToken();
        match(this.token, TokenType.IDENTYFIER);
    }

    public void tipoVar() {
        if(
            this.token.getContent().intern() != Keyword.INT.toString().intern() 
            && this.token.getContent().intern() != Keyword.FLOAT.toString()
        ) {
            throw new SyntaxException("Expected INT or FLOAT and found " + token.getContent());  
        }
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
        termoAritmetico();
        this.token = this.scanner.nextToken();
        expressaoAritmetica2();
        this.token = this.scanner.nextToken();
        match(this.token, TokenType.DELIMITER);
	}

    public void expressaoAritmetica2() {
        expressaoAritmetica3();
        this.token = this.scanner.nextToken();
        if (this.token != null && this.token.getType() != TokenType.DELIMITER) {
            expressaoAritmetica2();
        }
	}

    public void expressaoAritmetica3() {
        if (this.token.getType() == TokenType.SUM_OP) {
            match(this.token, TokenType.SUM_OP);
            this.token = this.scanner.nextToken();
            termoAritmetico();
            return;
        }

        match(this.token, TokenType.SUB_OP);
        this.token = this.scanner.nextToken();
        termoAritmetico();
	}

	public void expressaoRelacional() {
        termoRelacional();
        this.token = this.scanner.nextToken();
        expressaoRelacional2();
        this.token = this.scanner.nextToken();
        match(this.token, TokenType.DELIMITER);
	}

    public void expressaoRelacional2() {
        operadorBooleano();
        this.token = this.scanner.nextToken();
        if (this.token != null && this.token.getType() != TokenType.DELIMITER) {
            expressaoRelacional();
        }
    }

    public void termoRelacional() {
        if (this.token.getType() == TokenType.LEFT_PARENTHESIS) {
            match(this.token, TokenType.LEFT_PARENTHESIS);
            this.token = this.scanner.nextToken();
            expressaoRelacional();
            this.token = this.scanner.nextToken();
            match(this.token, TokenType.RIGHT_PARENTHESIS);
            return;
        }

        expressaoAritmetica();
        this.token = this.scanner.nextToken();
        operadorRelacional();
        this.token = this.scanner.nextToken();
    }

    public void operadorRelacional() {
        match(this.token, TokenType.REL_OP);
    }

    public void operadorBooleano() {
        if(
            this.token.getContent().intern() != Keyword.AND.toString().intern() 
            && this.token.getContent().intern() != Keyword.OR.toString()
        ) {
            throw new SyntaxException("Expected AND or OR and found " + token.getContent());  
        }
    }

    public void termoAritmetico () {
        fatorAritmetico();
        this.token = this.scanner.nextToken();
        termoAritmetico2();
        this.token = this.scanner.nextToken();
        match(this.token, TokenType.DELIMITER);
    }

    public void termoAritmetico2 () {
        termoAritmetico3();
        this.token = this.scanner.nextToken();
        if (this.token != null && this.token.getType() != TokenType.DELIMITER) {
            termoAritmetico2();
        }
    }

    public void termoAritmetico3 () {
        if (this.token.getType() == TokenType.MULT_OP) {
            match(this.token, TokenType.MULT_OP);
            this.token = this.scanner.nextToken();
            fatorAritmetico();
            return;
        }

        match(this.token, TokenType.DIV_OP);
        this.token = this.scanner.nextToken();
        fatorAritmetico();
    }

    public void fatorAritmetico() {
        if (
            this.token.getContent().intern() != Keyword.INT.toString().intern() &&
            this.token.getContent().intern() != Keyword.FLOAT.toString().intern() &&
            this.token.getType() != TokenType.IDENTYFIER
        ) {
            if (this.token.getType() != TokenType.LEFT_PARENTHESIS) {
                throw new SyntaxException("Expected INT, FLOAT, IDENTYFIER or (EXPRESSION) and found " + token.getContent());  
            }

            match(this.token, TokenType.LEFT_PARENTHESIS);
            this.token = this.scanner.nextToken();
            expressaoAritmetica();
            this.token = this.scanner.nextToken();
            match(this.token, TokenType.RIGHT_PARENTHESIS);
        }

    } 
}
