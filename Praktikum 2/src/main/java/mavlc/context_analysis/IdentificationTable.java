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

import mavlc.syntax.statement.Declaration;

/*
 *
 * EiCB group number: 3
 * Names and matriculation numbers of all group members:
 * Alexander Heeg, 2359830
 * Julian Ewald, 2628600
 * Maximilian Michael Steinbach, 2664460
 */

/**
 * A table for identifiers used inside a function.
 */
public class IdentificationTable {
	private Scope scopes = new Scope(null);
	
	/**
	 * Declares the given identifier in the current scope.
	 *
	 * @param name the identifier to declare
	 * @param declaration the reference to the identifier's declaration site
	 */
	public void addIdentifier(String name, Declaration declaration) {
		scopes.addIdentifier(name, declaration);
	}
	
	/**
	 * Looks up the innermost declaration of the given identifier.
	 *
	 * @param name the identifier to look up
	 * @return the identifier's innermost declaration site
	 */
	public Declaration getDeclaration(String name) {
		return scopes.getDeclaration(name);
	}
	
	/**
	 * Opens a new scope.
	 */
	public void openNewScope() {
		scopes = new Scope(scopes);
	}
	
	/**
	 * Closes the current scope.
	 */
	public void closeCurrentScope() {
		scopes = scopes.parentScope;
	}
}
