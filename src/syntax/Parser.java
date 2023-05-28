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

    public void toNextToken() {
        this.token = this.scanner.nextToken();
        // System.out.println(this.token.getContent() + " Tipo: " + this.token.getType() + " line: " + this.token.getLine() + " column: " + this.token.getColumn());
    }

	public void match(Token token, TokenType type) {
		if (this.token != null) {
			if (token.getType() != type) {
                System.out.println();
                this.toNextToken();
				throw new SyntaxException("Type " + type + " expected, found " + token.getType() + " with value: " +  token.getContent() + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
			}
		} else {
			throw new SyntaxException("Type " + type + " expected, found null"  + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
		}

	}

	public void match(Token token, Keyword keyword) {
		if(this.token != null) {
			if (token.getContent().intern() != keyword.toString().intern()) {
				throw new SyntaxException("Keyword " + keyword + " expected, found " + token.getContent() + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
			}
		} else {
			throw new SyntaxException("Keyword " + keyword + " expected, found null" + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
		}

	}

	public void Programa() {
		this.toNextToken();
		match(this.token, TokenType.TWO_POINTS);
		this.toNextToken();
		match(this.token, Keyword.DECLARACOES);

		listaDeclaracoes(null);	

		this.toNextToken();
		match(this.token, TokenType.DELIMITER);

		this.toNextToken();
		match(this.token, TokenType.TWO_POINTS);
		this.toNextToken();
		match(this.token, Keyword.ALGORITMO);

		listaComandos(null);

		this.toNextToken();
		match(this.token, TokenType.DELIMITER);
	}

	public void listaDeclaracoes(Token tokenprop) {
        if (tokenprop == null) {
            this.toNextToken();
        }

        declaracao();
        listaDeclaracoes2();

        match(this.token, TokenType.DELIMITER);
	}

    public void listaDeclaracoes2() {
        // verifica o token seguinte:
        this.toNextToken();

        if (this.token.getType() != TokenType.DELIMITER) {
            listaDeclaracoes(this.token);
            this.toNextToken();
        }
    }

    public void declaracao() {
        tipoVar();
        this.toNextToken();
        match(this.token, TokenType.TWO_POINTS);
        this.toNextToken();
        match(this.token, TokenType.IDENTYFIER);
    }

    public void tipoVar() {
        if(
            this.token.getContent().intern() != Keyword.INT.toString().intern() 
            && this.token.getContent().intern() != Keyword.FLOAT.toString()
        ) {
            throw new SyntaxException("Expected INT or FLOAT and found " + token.getContent() + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");  
        }
    }

	public void listaComandos(Token token) {
		comando(token);
		listaComandos2();
		match(this.token, TokenType.DELIMITER);
	}

	public void listaComandos2() {
		this.toNextToken();
		if (this.token != null && this.token.getType() != TokenType.DELIMITER) {
			listaComandos(this.token);
		} 
	}

	public void comando(Token tokenProp) {
		if (tokenProp == null) {
			this.toNextToken();
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
            throw new SyntaxException("Expected ASSING, INPUT, PRINT, IF or WHILE and found " + token.getContent() + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
        }
	}

	public void comandoAtribuicao() {
        this.toNextToken();
		expressaoAritmetica();
		this.toNextToken();
		match(this.token, Keyword.TO);
		this.toNextToken();
		match(this.token, TokenType.IDENTYFIER);
	}

	public void comandoEntrada() {
		this.toNextToken();
		match(this.token, TokenType.IDENTYFIER);
	}

	public void comandoSaida() {
		this.toNextToken();
		if (token.getType() != TokenType.IDENTYFIER && token.getType() != TokenType.STRING) {
			throw new SyntaxException("Identyfier or String expected, found " + token.getType() + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
		}
	}
	
	public void comandoCondicao() {
		expressaoRelacional(null);
		this.toNextToken();
		match(this.token, Keyword.THEN);
		comando(null);
		comandoCondicao2();


		match(this.token, TokenType.DELIMITER);
	}

	public void comandoCondicao2() {
		this.toNextToken();
		if (token != null && token.getType() != TokenType.DELIMITER) {
			match(this.token, Keyword.ELSE);
			comando(null);
            this.toNextToken();
		}
	}

	public void comandoRepeticao() {
		expressaoRelacional(null);
		comando(null);
	}

	public void expressaoAritmetica() {
        termoAritmetico();
        expressaoAritmetica2();
        match(this.token, TokenType.DELIMITER);
	}

    public void expressaoAritmetica2() {
        this.toNextToken();
        if (this.token != null && (this.token.getType() == TokenType.SUM_OP || this.token.getType() == TokenType.SUB_OP)) {
            expressaoAritmetica3();
            expressaoAritmetica2();
        } 
	}

    public void expressaoAritmetica3() {
        if (this.token.getType() == TokenType.SUM_OP) {
            match(this.token, TokenType.SUM_OP);
            this.toNextToken();
            termoAritmetico();
            return;
        }

        match(this.token, TokenType.SUB_OP);
        this.toNextToken();
        termoAritmetico();
	}

	public void expressaoRelacional(Token token) {
        termoRelacional(token);
        expressaoRelacional2();
        match(this.token, TokenType.DELIMITER);
	}

    public void expressaoRelacional2() {
        this.toNextToken();
        if (this.token != null && this.token.getType() != TokenType.DELIMITER && (this.token.getContent().intern() == Keyword.AND.toString().intern() || this.token.getContent().intern() == Keyword.OR.toString())) {
            operadorBooleano();
            expressaoRelacional(token);
        }
    }

    public void termoRelacional(Token tokenprop) {
        if (tokenprop == null) {
            this.toNextToken();
        }

        if (this.token.getType() == TokenType.LEFT_PARENTHESIS) {
            match(this.token, TokenType.LEFT_PARENTHESIS);
            this.toNextToken();
            expressaoRelacional(token);
            this.toNextToken();
            match(this.token, TokenType.RIGHT_PARENTHESIS);
            return;
        }

        expressaoAritmetica();
        operadorRelacional();

        this.toNextToken();
        expressaoAritmetica();
    }

    public void operadorRelacional() {
        rel_op();
    }

    private void rel_op() {
        this.toNextToken();
        TokenType type = this.token.getType();
        if (type != TokenType.LESS_OP && type != TokenType.LESS_EQUALS_OP && type != TokenType.EQUALS_OP && type != TokenType.GREATER_OP && type != TokenType.GREATER_EQUALS_OP && type != TokenType.DIF_OP) {
            throw new SyntaxException("Type REL_OP expected, found " + token.getType() + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
        }
    }

    public void operadorBooleano() {
        if(
            this.token.getContent().intern() != Keyword.AND.toString().intern() 
            && this.token.getContent().intern() != Keyword.OR.toString()
        ) {
            throw new SyntaxException("Expected AND or OR and found " + token.getContent() + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");  
        }
    }

    public void termoAritmetico () {
        fatorAritmetico();
        termoAritmetico2();
        match(this.token, TokenType.DELIMITER);
    }

    public void termoAritmetico2 () {
        this.toNextToken();
        if (this.token != null && (this.token.getType() == TokenType.MULT_OP || this.token.getType() == TokenType.DIV_OP)) {
            termoAritmetico3();
            termoAritmetico2();
        }

    }

    public void termoAritmetico3 () {
        if (this.token.getType() == TokenType.MULT_OP) {
            match(this.token, TokenType.MULT_OP);
            this.toNextToken();
            fatorAritmetico();
            return;
        }

        match(this.token, TokenType.DIV_OP);
        this.toNextToken();
        fatorAritmetico();
    }

    public void fatorAritmetico() {
        if (
            this.token.getType() != TokenType.NUMBER &&
            this.token.getType() != TokenType.IDENTYFIER
        ) {
            if (this.token.getType() != TokenType.LEFT_PARENTHESIS) {
                throw new SyntaxException("Expected INT, FLOAT, IDENTYFIER or (EXPRESSION) and found " + token.getContent() + ": " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");  
            }

            match(this.token, TokenType.LEFT_PARENTHESIS);
            this.toNextToken();
            expressaoAritmetica();
            this.toNextToken();
            match(this.token, TokenType.RIGHT_PARENTHESIS);
        }
    } 
}
