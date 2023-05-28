package main;

import exceptions.LexicalException;
import exceptions.SyntaxException;
import lexical.Scanner;
import lexical.Token;
import syntax.Parser;
import syntax.ParserV2;

public class Main {
	
	public static void main(String[] args) {
		try {
			// Scanner scanner = new Scanner("codigo_fonte_que_deve_rodar_apos_a_implementacao.mc");
			Scanner scanner = new Scanner("source_code.mc");
			// Parser parser = new Parser(scanner);
			ParserV2 parser = new ParserV2(scanner);

			parser.Programa();
			System.out.println("Compilation successful!");
		} catch (LexicalException e) {
			System.out.println("Lexical error: " + e.getMessage());
		} catch (SyntaxException e) {
			System.out.println("Syntax error: " + e.getMessage());
		} catch (Exception e) {
			e.printStackTrace();
		}
	}


}
