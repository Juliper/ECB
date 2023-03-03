/*******************************************************************************
 * Copyright (c) 2016-2019 Embedded Systems and Applications Group
 * Department of Computer Science, Technische Universitaet Darmstadt,
 * Hochschulstr. 10, 64289 Darmstadt, Germany.
 *
 * All rights reserved.
 *
 * This software is provided free for educational use only.
 * It may not be used for commercial purposes without the
 * prior written permission of the authors.
 ******************************************************************************/
package mavlc.parsing;

import mavlc.errors.SyntaxError;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.expression.*;
import mavlc.syntax.function.FormalParameter;
import mavlc.syntax.function.Function;
import mavlc.syntax.module.Module;
import mavlc.syntax.record.RecordElementDeclaration;
import mavlc.syntax.record.RecordTypeDeclaration;
import mavlc.syntax.statement.*;
import mavlc.syntax.type.*;
import mavlc.type.ValueType;

import java.util.*;

import static mavlc.parsing.Token.TokenType.*;

/* TODO enter group information
 *
 * EiCB group number: 3
 * Names and matriculation numbers of all group members:
 * Alexander Heeg, 2359830
 * Julian Ewald, 2628600
 * Maximilian Michael Steinbach, 2664460
 */

/**
 * A recursive-descent parser for MAVL.
 */
public final class Parser {
	
	private final Deque<Token> tokens;
	private Token currentToken;
	
	/**
	 * @param tokens A token stream that was produced by the {@link Scanner}.
	 */
	public Parser(Deque<Token> tokens) {
		this.tokens = tokens;
		currentToken = tokens.poll();
	}
	
	/**
	 * Parses the MAVL grammar's start symbol, Module.
	 *
	 * @return A {@link Module} node that is the root of the AST representing the tokenized input program.
	 * @throws SyntaxError to indicate that an unexpected token was encountered.
	 */
	public Module parse() {
		SourceLocation location = currentToken.sourceLocation;
		
		List<Function> functions = new ArrayList<>();
		List<RecordTypeDeclaration> records = new ArrayList<>();
		while(currentToken.type != EOF) {
			switch(currentToken.type) {
				case FUNCTION:
					functions.add(parseFunction());
					break;
				case RECORD:
					records.add(parseRecordTypeDeclaration());
					break;
				default:
					throw new SyntaxError(currentToken, FUNCTION, RECORD);
			}
		}
		return new Module(location, functions, records);
	}
	
	private String accept(Token.TokenType type) {
		Token t = currentToken;
		if(t.type != type)
			throw new SyntaxError(t, type);
		acceptIt();
		return t.spelling;
	}
	
	private void acceptIt() {
		currentToken = tokens.poll();
		if(currentToken == null || currentToken.type == ERROR)
			throw new SyntaxError(currentToken != null ? currentToken : new Token(EOF, null, -1, -1));
	}
	
	private Function parseFunction() {
		SourceLocation location = currentToken.sourceLocation;

		accept(FUNCTION);
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		
		List<FormalParameter> parameters = new ArrayList<>();
		List<Statement> body = new ArrayList<>();
		
		accept(LPAREN);
		if(currentToken.type != RPAREN) {
			parameters.add(parseFormalParameter());
			while(currentToken.type != RPAREN) {
				accept(COMMA);
				parameters.add(parseFormalParameter());
			}
		}
		accept(RPAREN);
		
		accept(LBRACE);
		while(currentToken.type != RBRACE)
			body.add(parseStatement());
		accept(RBRACE);
		
		return new Function(location, name, typeSpecifier, parameters, body);
	}
	
	private FormalParameter parseFormalParameter() {
		SourceLocation location = currentToken.sourceLocation;
		
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		
		return new FormalParameter(location, name, typeSpecifier);
	}
	
	private RecordTypeDeclaration parseRecordTypeDeclaration() {
		SourceLocation location = currentToken.sourceLocation;
		accept( RECORD );
		String id = accept( ID );
		accept( LBRACE );
		List<RecordElementDeclaration> records = new ArrayList<>();
		do
		{
			records.add( parseRecordElementDeclaration() );
		} while( currentToken.type != RBRACE );
		acceptIt();
		return new RecordTypeDeclaration( location, id, records );
	}
	
	private RecordElementDeclaration parseRecordElementDeclaration() {
		SourceLocation location = currentToken.sourceLocation;
		boolean isVar = false;
		switch( currentToken.type )
		{
			case VAR:
				isVar = true;
				acceptIt();
				break;
			case VAL:
				acceptIt();
				break;
			default:
				throw new SyntaxError( currentToken, VAR, VAL );
		}
		TypeSpecifier type = parseTypeSpecifier();
		String id = accept( ID );
		accept( SEMICOLON );
		return new RecordElementDeclaration( location, isVar, type, id );
	}
	
	private IteratorDeclaration parseIteratorDeclaration() {
		SourceLocation location = currentToken.sourceLocation;
		
		boolean isVariable;
		switch(currentToken.type) {
			case VAL:
				accept(VAL);
				isVariable = false;
				break;
			case VAR:
				accept(VAR);
				isVariable = true;
				break;
			default:
				throw new SyntaxError(currentToken, VAL, VAR);
		}
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		return new IteratorDeclaration(location, name, typeSpecifier, isVariable);
	}
	
	private TypeSpecifier parseTypeSpecifier() {
		SourceLocation location = currentToken.sourceLocation;
		
		boolean vector = false;
		switch(currentToken.type) {
			case INT:
				acceptIt();
				return new IntTypeSpecifier(location);
			case FLOAT:
				acceptIt();
				return new FloatTypeSpecifier(location);
			case BOOL:
				acceptIt();
				return new BoolTypeSpecifier(location);
			case VOID:
				acceptIt();
				return new VoidTypeSpecifier(location);
			case STRING:
				acceptIt();
				return new StringTypeSpecifier(location);
			case VECTOR:
				accept(VECTOR);
				vector = true;
				break;
			case MATRIX:
				accept(MATRIX);
				break;
			case ID:
				String name = accept(ID);
				return new RecordTypeSpecifier(location, name);
			default:
				throw new SyntaxError(currentToken, INT, FLOAT, BOOL, VOID, STRING, VECTOR, MATRIX, ID);
		}
		
		accept(LANGLE);
		TypeSpecifier subtype;
		switch(currentToken.type) {
			case INT:
				subtype = new IntTypeSpecifier(currentToken.sourceLocation);
				break;
			case FLOAT:
				subtype = new FloatTypeSpecifier(currentToken.sourceLocation);
				break;
			default:
				throw new SyntaxError(currentToken, INT, FLOAT);
		}
		acceptIt();
		accept(RANGLE);
		accept(LBRACKET);
		Expression x = parseExpr();
		accept(RBRACKET);
		
		if(vector)
			return new VectorTypeSpecifier(location, subtype, x);
		
		accept(LBRACKET);
		Expression y = parseExpr();
		accept(RBRACKET);
		
		return new MatrixTypeSpecifier(location, subtype, x, y);
	}
	
	private Statement parseStatement() {
		switch(currentToken.type) {
			case VAL:
				return parseValueDef();
			case VAR:
				return parseVarDecl();
			case RETURN:
				return parseReturn();
			case ID:
				return parseAssignOrCall();
			case FOR:
				return parseFor();
			case FOREACH:
				return parseForEach();
			case IF:
				return parseIf();
			case SWITCH:
				return parseSwitch();
			case LBRACE:
				return parseCompound();
			default:
				throw new SyntaxError(currentToken, VAL, VAR, RETURN, ID, FOR, FOREACH, IF, SWITCH, LBRACE);
		}
	}
	
	private ValueDefinition parseValueDef() {
		SourceLocation location = currentToken.sourceLocation;
		accept( VAL );
		TypeSpecifier type = parseTypeSpecifier();
		String id = accept( ID );
		accept( ASSIGN );
		Expression expr = parseExpr();
		accept( SEMICOLON );
		return new ValueDefinition( location, type, id, expr );
	}
	
	private VariableDeclaration parseVarDecl() {
		SourceLocation location = currentToken.sourceLocation;
		accept( VAR );
		TypeSpecifier type = parseTypeSpecifier();
		String id = accept( ID );
		accept( SEMICOLON );
		return new VariableDeclaration( location, type, id );
	}
	
	private ReturnStatement parseReturn() {
		SourceLocation location = currentToken.sourceLocation;
		accept(RETURN);
		Expression e = parseExpr();
		accept(SEMICOLON);
		
		return new ReturnStatement(location, e);
	}
	
	private Statement parseAssignOrCall() {
		SourceLocation location = currentToken.sourceLocation;
		
		String name = accept(ID);
		
		Statement s;
		if(currentToken.type != LPAREN)
			s = parseAssign(name, location);
		else
			s = new CallStatement(location, parseCall(name, location));
		accept(SEMICOLON);
		
		return s;
	}
	
	private VariableAssignment parseAssign(String name, SourceLocation location) {
		LeftHandIdentifier leftHandIdentifier;
		if(currentToken.type == AT)
		{
			acceptIt();
			String elementName = accept( ID );
			leftHandIdentifier = new RecordLhsIdentifier( location, name, elementName );
		}
		else if(currentToken.type == LBRACKET)
		{
			acceptIt();
			Expression rowIndex = parseExpr();
			accept( RBRACKET );
			if( currentToken.type == LBRACKET )
			{
				acceptIt();
				Expression colIndex = parseExpr();
				accept( RBRACKET );
				leftHandIdentifier = new MatrixLhsIdentifier( location, name, rowIndex, colIndex );
			}
			else
			{
				leftHandIdentifier = new VectorLhsIdentifier( location, name, rowIndex );
			}
		}
		else
		{
			leftHandIdentifier = new LeftHandIdentifier( location, name );
		}
		accept( ASSIGN );
		Expression assignValue = parseExpr();
		return new VariableAssignment( location, leftHandIdentifier, assignValue );
	}
	
	private CallExpression parseCall(String name, SourceLocation location) {
		accept(LPAREN);
		
		List<Expression> actualParameters = new ArrayList<>();
		if(currentToken.type != RPAREN) {
			actualParameters.add(parseExpr());
			while(currentToken.type != RPAREN) {
				accept(COMMA);
				actualParameters.add(parseExpr());
			}
		}
		accept(RPAREN);
		
		return new CallExpression(location, name, actualParameters);
	}
	
	private ForLoop parseFor() {
		SourceLocation location = currentToken.sourceLocation;
		accept( FOR );
		accept( LPAREN );
		String initVarName = accept( ID );
		accept( ASSIGN );
		Expression initExpression = parseExpr();
		accept( SEMICOLON );
		Expression loopCondition = parseExpr();
		accept( SEMICOLON );
		String incrVarName = accept( ID );
		accept( ASSIGN );
		Expression incrExpression = parseExpr();
		accept( RPAREN );
		Statement body = parseStatement();
		return new ForLoop( location, initVarName, initExpression, loopCondition, incrVarName, incrExpression, body );
	}
	
	private ForEachLoop parseForEach() {
		SourceLocation location = currentToken.sourceLocation;

		accept(FOREACH);
		accept(LPAREN);
		IteratorDeclaration param = parseIteratorDeclaration();
		accept(COLON);
		Expression struct = parseExpr();
		accept(RPAREN);
		return new ForEachLoop(location, param, struct, parseStatement());
	}
	
	private IfStatement parseIf() {
		SourceLocation location = currentToken.sourceLocation;
		accept(IF);
		accept(LPAREN);
		Expression test = parseExpr();
		accept(RPAREN);
		Statement then = parseStatement();
		if(currentToken.type == ELSE) {
			acceptIt();
			return new IfStatement(location, test, then, parseStatement());
		}
		return new IfStatement(location, test, then);
	}
	
	private SwitchStatement parseSwitch() {
		SourceLocation location = currentToken.sourceLocation;
		accept( SWITCH );
		accept( LPAREN );
		Expression condition = parseExpr();
		accept( RPAREN );
		accept( LBRACE );
		List<Case> cases = new ArrayList<>();
		List<Default> defaults = new ArrayList<>();
		while( currentToken.type != RBRACE )
		{
			switch( currentToken.type )
			{
				case CASE:
					cases.add( parseCase() );
					break;
				case DEFAULT:
					defaults.add( parseDefault() );
					break;
				default:
					throw new SyntaxError( currentToken, CASE, DEFAULT );
			}
		}
		acceptIt();
		return new SwitchStatement( location, condition, cases, defaults );
	}
	
	private Case parseCase() {
		SourceLocation location = currentToken.sourceLocation;
		accept( CASE );
		Expression condition = parseExpr();
		accept( COLON );
		Statement statement = parseStatement();
		return new Case( location, condition, statement );
	}
	
	private Default parseDefault() {
		SourceLocation location = currentToken.sourceLocation;
		accept( DEFAULT );
		accept( COLON );
		Statement statement = parseStatement();
		return new Default( location, statement );
	}
	
	private CompoundStatement parseCompound() {
		SourceLocation location = currentToken.sourceLocation;
		accept(LBRACE);
		List<Statement> statements = new ArrayList<>();
		while( currentToken.type != RBRACE )
		{
			statements.add( parseStatement() );
		}
		acceptIt();
		return new CompoundStatement( location, statements );
	}
	
	private Expression parseExpr() {
		return parseSelect();
	}
	
	private Expression parseSelect() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression cond = parseOr();
		if(currentToken.type == QMARK) {
			acceptIt();
			Expression trueCase = parseOr();
			accept(COLON);
			Expression falseCase = parseOr();
			return new SelectExpression(location, cond, trueCase, falseCase);
		}
		return cond;
	}
	
	private Expression parseOr() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseAnd();
		while(currentToken.type == OR) {
			acceptIt();
			x = new Or(location, x, parseAnd());
		}
		return x;
	}
	
	private Expression parseAnd() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseNot();
		while(currentToken.type == AND) {
			acceptIt();
			x = new And(location, x, parseNot());
		}
		return x;
	}
	
	private Expression parseNot() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == NOT) {
			acceptIt();
			return new Not(location, parseCompare());
		}
		return parseCompare();
	}
	
	private Expression parseCompare() {
		SourceLocation location = currentToken.sourceLocation;
		Expression expr = parseAddSub();
		while( currentToken.type == RANGLE || currentToken.type == LANGLE
				|| currentToken.type == CMPLE || currentToken.type == CMPGE
				|| currentToken.type == CMPEQ || currentToken.type == CMPNE )
		{
			switch( currentToken.type )
			{
				case RANGLE:
					acceptIt();
					expr = new Compare( location, expr, parseAddSub(), Compare.Comparison.GREATER );
					break;
				case LANGLE:
					acceptIt();
					expr = new Compare( location, expr, parseAddSub(), Compare.Comparison.LESS );
					break;
				case CMPLE:
					acceptIt();
					expr = new Compare( location, expr, parseAddSub(), Compare.Comparison.LESS_EQUAL );
					break;
				case CMPGE:
					acceptIt();
					expr = new Compare( location, expr, parseAddSub(), Compare.Comparison.GREATER_EQUAL );
					break;
				case CMPEQ:
					acceptIt();
					expr = new Compare( location, expr, parseAddSub(), Compare.Comparison.EQUAL );
					break;
				case CMPNE:
					acceptIt();
					expr = new Compare( location, expr, parseAddSub(), Compare.Comparison.NOT_EQUAL );
					break;
			}
		}
		return expr;
	}
	
	private Expression parseAddSub() {
		SourceLocation location = currentToken.sourceLocation;
		Expression expr = parseMulDiv();
		while(currentToken.type == ADD || currentToken.type == SUB)
		{
			if(currentToken.type == ADD)
			{
				acceptIt();
				expr = new Addition(location, expr, parseUnaryMinus());
			}
			else
			{
				acceptIt();
				expr = new Subtraction(location, expr, parseUnaryMinus());
			}
		}
		return expr;
	}
	
	private Expression parseMulDiv() {
		SourceLocation location = currentToken.sourceLocation;
		Expression expr = parseUnaryMinus();
		while(currentToken.type == MULT || currentToken.type == DIV)
		{
			if(currentToken.type == MULT)
			{
				acceptIt();
				expr = new Multiplication(location, expr, parseUnaryMinus());
			}
			else
			{
				acceptIt();
				expr = new Division(location, expr, parseUnaryMinus());
			}
		}
		return expr;
	}
	
	private Expression parseUnaryMinus() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == SUB) {
			acceptIt();
			return new UnaryMinus(location, parseExponentiation());
		}
		return parseExponentiation();
	}
	
	private Expression parseExponentiation() {
		SourceLocation location = currentToken.sourceLocation;
		Expression base = parseDotProd();
		Expression exponent;
		SourceLocation sublocation;
		if(currentToken.type == EXP)
		{
			acceptIt();
			sublocation = currentToken.sourceLocation;
			exponent = parseDotProd();
		}
		else
		{
			return base;
		}
		while(currentToken.type == EXP)
		{
			acceptIt();
			SourceLocation tmp = sublocation;
			sublocation = currentToken.sourceLocation;
			exponent = new Exponentiation( tmp, exponent, parseDotProd() );
		}
		return new Exponentiation( location, base, exponent );
	}
	
	private Expression parseDotProd() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseMatrixMul();
		while(currentToken.type == DOTPROD) {
			acceptIt();
			x = new DotProduct(location, x, parseMatrixMul());
		}
		return x;
	}
	
	private Expression parseMatrixMul() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseTranspose();
		while(currentToken.type == MATMULT) {
			acceptIt();
			x = new MatrixMultiplication(location, x, parseTranspose());
		}
		return x;
	}
	
	private Expression parseTranspose() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == TRANSPOSE) {
			acceptIt();
			return new MatrixTranspose(location, parseDim());
		}
		return parseDim();
	}
	
	private Expression parseDim() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseSubRange();
		switch(currentToken.type) {
			case ROWS:
				acceptIt();
				return new MatrixRows(location, x);
			case COLS:
				acceptIt();
				return new MatrixCols(location, x);
			case DIM:
				acceptIt();
				return new VectorDimension(location, x);
			default:
				return x;
		}
	}
	
	private Expression parseSubRange() {
		SourceLocation location = currentToken.sourceLocation;
		Expression struct = parseElementSelect();
		if(currentToken.type == LBRACE)
		{
			acceptIt();
			Expression rowStartOffset = parseExpr();
			accept(COLON);
			Expression rowBase = parseExpr();
			accept( COLON );
			Expression rowEndOffSet = parseExpr();
			accept( RBRACE );
			if(currentToken.type == LBRACE)
			{
				acceptIt();
				Expression colStartOffSet = parseExpr();
				accept(COLON);
				Expression colBase = parseExpr();
				accept( COLON );
				Expression colEndOffSet = parseExpr();
				accept( RBRACE );
				return new SubMatrix( location, struct, rowBase, rowStartOffset, rowEndOffSet, colBase, colStartOffSet, colEndOffSet );
			}
			return new SubVector( location, struct, rowBase, rowStartOffset, rowEndOffSet );
		}
		return struct;
	}
	
	private Expression parseElementSelect() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseRecordElementSelect();
		
		while(currentToken.type == LBRACKET) {
			acceptIt();
			Expression idx = parseExpr();
			accept(RBRACKET);
			x = new ElementSelect(location, x, idx);
		}
		
		return x;
	}
	
	private Expression parseRecordElementSelect() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseAtom();
		
		if(currentToken.type == AT) {
			accept(AT);
			String elementName = accept(ID);
			x = new RecordElementSelect(location, x, elementName);
		}
		
		return x;
	}
	
	private Expression parseAtom() {
		SourceLocation location = currentToken.sourceLocation;
		
		switch(currentToken.type) {
			case INTLIT:
				return new IntValue(location, parseIntLit());
			case FLOATLIT:
				return new FloatValue(location, parseFloatLit());
			case BOOLLIT:
				return new BoolValue(location, parseBoolLit());
			case STRINGLIT:
				return new StringValue(location, accept(STRINGLIT));
			default: /* check other cases below */
		}
		
		if(currentToken.type == ID) {
			String name = accept(ID);
			if(currentToken.type != LPAREN) {
				return new IdentifierReference(location, name);
				
			} else {
				return parseCall(name, location);
			}
		}
		
		if(currentToken.type == LPAREN) {
			acceptIt();
			Expression x = parseExpr();
			accept(RPAREN);
			return x;
		}
		
		if(currentToken.type == AT) {
			acceptIt();
			String id = accept( ID );
			List<Expression> expressionList = parseInitializerList();
			return new RecordInit( location, id, expressionList );
		}
		
		if(currentToken.type == LBRACKET) {
			return new StructureInit(location, parseInitializerList());
		}
		
		throw new SyntaxError(currentToken, INTLIT, FLOATLIT, BOOLLIT, STRINGLIT, ID, LPAREN, LBRACKET, AT);
	}
	
	private List<Expression> parseInitializerList() {
		List<Expression> elements = new ArrayList<>();
		
		accept(LBRACKET);
		elements.add(parseExpr());
		while(currentToken.type == COMMA) {
			accept(COMMA);
			elements.add(parseExpr());
		}
		accept(RBRACKET);
		
		return elements;
	}
	
	private int parseIntLit() {
		return Integer.parseInt(accept(INTLIT));
	}
	
	private float parseFloatLit() {
		return Float.parseFloat(accept(FLOATLIT));
	}
	
	private boolean parseBoolLit() {
		return Boolean.parseBoolean(accept(BOOLLIT));
	}
}
