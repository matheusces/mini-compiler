package syntax;

import java.security.Key;

import exceptions.SyntaxException;
import lexical.Scanner;
import lexical.Token;
import utils.Keyword;
import utils.TokenType;

public class ParserV2 {
    private Scanner scanner;
	private Token token;

	public ParserV2(Scanner scanner) {
		this.scanner = scanner;
	}

    public void match(Token token, TokenType type) {
		if (this.token != null) {
			if (token.getType() != type) {
				throw new SyntaxException("Type " + type + " expected, found " + token.getType() + " with value: " +  token.getContent() + " : " + "[line:" + this.token.getLine()  + " ] [column:"+ this.token.getColumn() + "]");
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
		this.token = this.scanner.nextToken();
		match(this.token, TokenType.TWO_POINTS);
		this.token = this.scanner.nextToken();
		match(this.token, Keyword.DECLARACOES);

		listaDeclaracoes(null);	

		match(this.token, TokenType.TWO_POINTS);
		this.token = this.scanner.nextToken();
		match(this.token, Keyword.ALGORITMO);

		listaComandos(null);
	}

	public void listaDeclaracoes(Token tokenprop) {
        declaracao(tokenprop);
        this.token = this.scanner.nextToken();
        match(this.token, TokenType.DELIMITER);
        listaDeclaracoes2();
	}

    public void listaDeclaracoes2() {
        // verifica o token seguinte:
        this.token = this.scanner.nextToken();

        if (this.token.getType() != TokenType.TWO_POINTS) {
            listaDeclaracoes(this.token);
        }
    }

    public void declaracao(Token tokenprop) {
        if (tokenprop == null) {
            this.token = this.scanner.nextToken();
        }
        match(this.token, TokenType.IDENTYFIER);
        this.token = this.scanner.nextToken();
        match(this.token, TokenType.TWO_POINTS);
        tipoVar();
    }

    public void tipoVar() { 
        this.token = this.scanner.nextToken();
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
	}

	public void listaComandos2() {
		this.token = this.scanner.nextToken();
		if (this.token != null) {
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

        if (token != null) {
            match(this.token, TokenType.DELIMITER);
        }
	}

	public void comandoAtribuicao() {
        this.token = this.scanner.nextToken();
		expressaoAritmetica();
		match(this.token, Keyword.TO);
		this.token = this.scanner.nextToken();
		match(this.token, TokenType.IDENTYFIER);
        this.token = this.scanner.nextToken();
	}

	public void comandoEntrada() {
		this.token = this.scanner.nextToken();
		match(this.token, TokenType.IDENTYFIER);
        this.token = this.scanner.nextToken();

	}

	public void comandoSaida() {
		this.token = this.scanner.nextToken();
		if (token.getType() != TokenType.IDENTYFIER && token.getType() != TokenType.STRING) {
			throw new SyntaxException("Identyfier or String expected, found " + token.getType());
		}

        this.token = this.scanner.nextToken();
	}
	
	public void comandoCondicao() {
		expressaoRelacional(null);
		match(this.token, Keyword.THEN);
		comando(null);
		comandoCondicao2(null);
	}

	public void comandoCondicao2(Token tokenprop) {
        if (tokenprop == null) {
            this.token = this.scanner.nextToken();
        }
		if (token != null && token.getType() != TokenType.DELIMITER && token.getContent().intern() != Keyword.ELSE.toString().intern()) {
            comando(token);
            comandoCondicao2(null);
            return;
		}

        if (token != null && token.getType() != TokenType.DELIMITER) {
            match(this.token, Keyword.ELSE);
            comandoCondicao2(null);
        }



	}

	public void comandoRepeticao() {
		expressaoRelacional(null);
		comando(this.token);

        this.token = this.scanner.nextToken();
        if (this.token != null && this.token.getType() != TokenType.DELIMITER) {
            comando(this.token);
        }
	}

	public void expressaoAritmetica() {
        termoAritmetico();   //  2/4 , 4 x 3  // next Token chamado na ultima interação
        expressaoAritmetica2();
	}

    public void expressaoAritmetica2() {
        if (this.token != null && (this.token.getType() == TokenType.SUM_OP || this.token.getType() == TokenType.SUB_OP)) {
            expressaoAritmetica3();
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

	public void expressaoRelacional(Token token) {
        termoRelacional(token);
        expressaoRelacional2();
	}

    public void expressaoRelacional2() {
        if (this.token != null && (this.token.getContent().intern() == Keyword.AND.toString().intern() || this.token.getContent().intern() == Keyword.OR.toString())) {
            operadorBooleano();
            this.token = this.scanner.nextToken();
            expressaoRelacional(token);
        }
    }

    public void termoRelacional(Token tokenprop) {
        if (tokenprop == null) {
            this.token = this.scanner.nextToken();
        }

        if (this.token.getType() == TokenType.LEFT_PARENTHESIS) {
            match(this.token, TokenType.LEFT_PARENTHESIS);
            this.token = this.scanner.nextToken();
            expressaoRelacional(token);
            this.token = this.scanner.nextToken();
            match(this.token, TokenType.RIGHT_PARENTHESIS);
            return;
        }

        expressaoAritmetica();
        operadorRelacional();

        this.token = this.scanner.nextToken();
        expressaoAritmetica();
    }

    public void operadorRelacional() {
        rel_op();
    }

    private void rel_op() {
        TokenType type = this.token.getType();
        if (type != TokenType.LESS_OP && type != TokenType.LESS_EQUALS_OP && type != TokenType.EQUALS_OP && type != TokenType.GREATER_OP && type != TokenType.GREATER_EQUALS_OP && type != TokenType.DIF_OP) {
            throw new SyntaxException("Type REL_OP expected, found " + token.getType() + " with value: " + token.getContent());
        }
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
        termoAritmetico2();
    }

    // looks good
    public void termoAritmetico2 () {
        this.token = this.scanner.nextToken();
        if (this.token != null && (this.token.getType() == TokenType.MULT_OP || this.token.getType() == TokenType.DIV_OP)) {
            termoAritmetico3();
            termoAritmetico2();
        }

    }

    // looks good
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

    // Looks good
    public void fatorAritmetico() {
        if (
            this.token.getType() != TokenType.NUMBER &&
            this.token.getType() != TokenType.IDENTYFIER
        ) {
            if (this.token.getType() != TokenType.LEFT_PARENTHESIS) {
                throw new SyntaxException("Expected INT, FLOAT, IDENTYFIER or (EXPRESSION) and found " + token.getContent());  
            }

            match(this.token, TokenType.LEFT_PARENTHESIS);
            this.token = this.scanner.nextToken();
            expressaoAritmetica();
            match(this.token, TokenType.RIGHT_PARENTHESIS);
        }
    } 
}
