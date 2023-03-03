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
package mavlc.context_analysis;

import mavlc.errors.NonConstantExpressionError;
import mavlc.syntax.AstNode;
import mavlc.syntax.AstNodeBaseVisitor;
import mavlc.syntax.expression.*;
import mavlc.syntax.statement.Declaration;
import mavlc.type.Type;

/*
 *
 * EiCB group number: 3
 * Names and matriculation numbers of all group members:
 * Alexander Heeg, 2359830
 * Julian Ewald, 2628600
 * Maximilian Michael Steinbach, 2664460
 */

public class ConstantExpressionEvaluator extends AstNodeBaseVisitor<Integer, Void> {
	@Override
	protected Integer defaultOperation(AstNode node, Void obj) {
		if(node instanceof Expression) {
			throw new NonConstantExpressionError((Expression) node);
		} else {
			throw new RuntimeException("Internal compiler error: should not try to constant-evaluate non-expressions");
		}
	}
	
	@Override
	public Integer visitIntValue(IntValue intValue, Void __) {
		return intValue.value;
	}
	
	public Integer visitAddition(Addition addition, Void __)
	{
		Integer lhc = addition.leftOperand.accept( this );
		Integer rhc = addition.rightOperand.accept( this );
		return lhc + rhc;
	}

	public Integer visitSubtraction(Subtraction subtraction, Void __)
	{
		Integer lhc = subtraction.leftOperand.accept( this );
		Integer rhc = subtraction.rightOperand.accept( this );
		return lhc - rhc;
	}

	public Integer visitMultiplication(Multiplication multiplication, Void __)
	{
		Integer lhc = multiplication.leftOperand.accept( this );
		Integer rhc = multiplication.rightOperand.accept( this );
		return lhc * rhc;
	}

	public Integer visitDivision(Division division, Void __)
	{
		Integer lhc = division.leftOperand.accept( this );
		Integer rhc = division.rightOperand.accept( this );
		return lhc / rhc;
	}

	public Integer visitExponentiation( Exponentiation exponentiation, Void __)
	{
		Integer lhc = exponentiation.leftOperand.accept( this );
		Integer rhc = exponentiation.rightOperand.accept( this );
		return (int) Math.pow( lhc, rhc );
	}

	public Integer visitUnaryMinus(UnaryMinus unaryMinus, Void __)
	{
		Integer lhc = unaryMinus.accept( this );
		return (-lhc);
	}

//	public Integer visitAnd( And and, Void __)
//	{
//		Integer lhc = and.leftOperand.accept( this );
//		Integer rhc = and.rightOperand.accept( this );
//		return lhc & rhc;
//	}
//
//	public Integer visitOr( Or or, Void __)
//	{
//		Integer lhc = or.leftOperand.accept( this );
//		Integer rhc = or.rightOperand.accept( this );
//		return lhc | rhc;
//	}
}
