/***************************************************************************
                    ansigenerator.cpp  -  description
                             -------------------
    begin                : Jul 5 2004
    copyright            : (C) 2004 by André Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "ansigenerator.h"

using namespace std;

namespace highlight {


string  AnsiGenerator::getOpenTag(const string&font,
                                  const string&fgCol, const string&bgCol) {
    ostringstream s;
    s  << "\033["<<font;
    if (!fgCol.empty())
        s<<";"<<fgCol;
    if (!bgCol.empty())
        s<<";"<<bgCol;
    s << "m";
    return  s.str();
}


AnsiGenerator::AnsiGenerator(const string &colourTheme)
        : CodeGenerator(colourTheme) {
    styleTagOpen.push_back("");
    styleTagOpen.push_back(getOpenTag("00", "31")); //str
    styleTagOpen.push_back(getOpenTag("00", "34"));//number
    styleTagOpen.push_back(getOpenTag("00", "34"));//sl comment
    styleTagOpen.push_back(getOpenTag("00", "34"));//ml comment
    styleTagOpen.push_back(getOpenTag("00", "35"));//escapeChar
    styleTagOpen.push_back(getOpenTag("00", "35"));//directive
    styleTagOpen.push_back(getOpenTag("01", "31"));//directive string
    styleTagOpen.push_back(getOpenTag("00", "30"));//linenum
    styleTagOpen.push_back(getOpenTag("01", "00"));//symbol

    styleTagClose.push_back("");
    for (int i=1;i<NUMBER_BUILTIN_STYLES; i++) {
        styleTagClose.push_back("\033[m");
    }
    newLineTag = "\n";
    spacer = " ";
}

AnsiGenerator::AnsiGenerator() {}
AnsiGenerator::~AnsiGenerator() {}

string AnsiGenerator::getHeader(const string & title) {
    return string();
}

void AnsiGenerator::printBody() {
    processRootState();
}

string AnsiGenerator::getFooter() {
    return string();
}

string AnsiGenerator::maskCharacter(unsigned char c) {
    string m;
    m+=c;
    return m;
}

string AnsiGenerator::getMatchingOpenTag(unsigned int styleID) {
    return (styleID)?getOpenTag("01", "32", ""):getOpenTag("00", "33");
}

string AnsiGenerator::getMatchingCloseTag(unsigned int styleID) {
    return "\033[m";
}

}
/***************************************************************************
                         ansicode.h  -  description
                             -------------------
    begin                : Jul 5 2004
    copyright            : (C) 2004 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef ANSIGENERATOR_H
#define ANSIGENERATOR_H

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

#include "codegenerator.h"
#include "charcodes.h"
#include "version.h"

namespace highlight {

/**
   \brief This class generates ANSI escape sequences.

   It contains information about the resulting document structure (document
   header and footer), the colour system, white space handling and text
   formatting attributes.

* @author Andre Simon
*/

class AnsiGenerator : public highlight::CodeGenerator
  {
  public:

   /** Constructor
     \param colourTheme Name of Colour theme to use
    */
    AnsiGenerator( const string &colourTheme);
    AnsiGenerator();
    ~AnsiGenerator();

   /** prints document header
       \param  title Title of the document
    */
    string getHeader(const string & title);

    /** Prints document footer*/
    string getFooter();

    /** Prints document body*/
    void printBody();

  private:

    /** \return escaped character*/
    virtual string maskCharacter(unsigned char );


    /** gibt ANSI-"Tags" zurueck (Farbindex+bold+kursiv)*/
    string getOpenTag(const string&font,
                      const string&fgCol, const string&bgCol="");



    string getMatchingOpenTag(unsigned int styleID);
    string getMatchingCloseTag(unsigned int styleID);
  };

}
#endif
/*
 * Copyright (c) 1998,1999,2000,2001,2002 Tal Davidson. All rights reserved.
 *
 * ASBeautifier.cpp
 * by Tal Davidson (davidsont@bigfoot.com)
 * This file is a part of "Artistic Style" - an indentater and reformatter
 * of C, C, C# and Java source files.
 *
 * The "Artistic Style" project, including all files needed to compile it,
 * is free software; you can redistribute it and/or use it and/or modify it
 * under the terms of the GNU General Public License as published 
 * by the Free Software Foundation; either version 2 of the License, 
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.
 *
 * Patches:
 * 18 March 1999 - Brian Rampel -
 *       Fixed inverse insertion of spaces vs. tabs when in -t mode.
 * 08 may 2004 
 *       applied ASBeautifier.cpp.BITFIELD.patch.bz2
 */

#include "compiler_defines.h"
#include "ASBeautifier.h"

#include <vector>
#include <string>
#include <cctype>
#include <algorithm>
#include <iostream>


#define INIT_CONTAINER(container, value)     {if ( (container) != NULL ) delete (container); (container) = (value); }
#define DELETE_CONTAINER(container)          {if ( (container) != NULL ) delete (container) ; }

#ifdef USES_NAMESPACE
using namespace std;
#endif




#ifdef USES_NAMESPACE
namespace astyle
  {
#endif

  bool ASBeautifier::calledInitStatic = false;

  vector<const string*> ASBeautifier::headers;
  vector<const string*> ASBeautifier::nonParenHeaders;
  vector<const string*> ASBeautifier::preBlockStatements;
  vector<const string*> ASBeautifier::assignmentOperators;
  vector<const string*> ASBeautifier::nonAssignmentOperators;

  /*
   * initialize the static vars
   */
  void ASBeautifier::initStatic()
  {
    if (calledInitStatic)
      return;

    calledInitStatic = true;

    headers.push_back(&AS_IF);
    headers.push_back(&AS_ELSE);
    headers.push_back(&AS_FOR);
    headers.push_back(&AS_WHILE);
    headers.push_back(&AS_DO);
    headers.push_back(&AS_TRY);
    headers.push_back(&AS_CATCH);
    headers.push_back(&AS_FINALLY);
    headers.push_back(&AS_SYNCHRONIZED);
    headers.push_back(&AS_SWITCH);
    headers.push_back(&AS_CASE);
    headers.push_back(&AS_DEFAULT);
    headers.push_back(&AS_FOREACH);
    headers.push_back(&AS_LOCK);
    headers.push_back(&AS_UNSAFE);
    headers.push_back(&AS_FIXED);
    headers.push_back(&AS_GET);
    headers.push_back(&AS_SET);
    headers.push_back(&AS_ADD);
    headers.push_back(&AS_REMOVE);
    //headers.push_back(&AS_PUBLIC);
    //headers.push_back(&AS_PRIVATE);
    //headers.push_back(&AS_PROTECTED);

    //headers.push_back(&AS_OPERATOR);
    headers.push_back(&AS_TEMPLATE);
    headers.push_back(&AS_CONST);
    /**/
    headers.push_back(&AS_STATIC);
    headers.push_back(&AS_EXTERN);

    nonParenHeaders.push_back(&AS_ELSE);
    nonParenHeaders.push_back(&AS_DO);
    nonParenHeaders.push_back(&AS_TRY);
    nonParenHeaders.push_back(&AS_FINALLY);
    nonParenHeaders.push_back(&AS_STATIC);
    nonParenHeaders.push_back(&AS_CONST);
    nonParenHeaders.push_back(&AS_EXTERN);
    nonParenHeaders.push_back(&AS_CASE);
    nonParenHeaders.push_back(&AS_DEFAULT);
    nonParenHeaders.push_back(&AS_UNSAFE);
    nonParenHeaders.push_back(&AS_GET);
    nonParenHeaders.push_back(&AS_SET);
    nonParenHeaders.push_back(&AS_ADD);
    nonParenHeaders.push_back(&AS_REMOVE);



    nonParenHeaders.push_back(&AS_PUBLIC);
    nonParenHeaders.push_back(&AS_PRIVATE);
    nonParenHeaders.push_back(&AS_PROTECTED);
    nonParenHeaders.push_back(&AS_TEMPLATE);
    nonParenHeaders.push_back(&AS_CONST);
    ///    nonParenHeaders.push_back(&AS_ASM);

    preBlockStatements.push_back(&AS_CLASS);
    preBlockStatements.push_back(&AS_STRUCT);
    preBlockStatements.push_back(&AS_UNION);
    preBlockStatements.push_back(&AS_INTERFACE);
    preBlockStatements.push_back(&AS_NAMESPACE);
    preBlockStatements.push_back(&AS_THROWS);
    preBlockStatements.push_back(&AS_EXTERN);

    assignmentOperators.push_back(&AS_ASSIGN);
    assignmentOperators.push_back(&AS_PLUS_ASSIGN);
    assignmentOperators.push_back(&AS_MINUS_ASSIGN);
    assignmentOperators.push_back(&AS_MULT_ASSIGN);
    assignmentOperators.push_back(&AS_DIV_ASSIGN);
    assignmentOperators.push_back(&AS_MOD_ASSIGN);
    assignmentOperators.push_back(&AS_OR_ASSIGN);
    assignmentOperators.push_back(&AS_AND_ASSIGN);
    assignmentOperators.push_back(&AS_XOR_ASSIGN);
    assignmentOperators.push_back(&AS_GR_GR_GR_ASSIGN);
    assignmentOperators.push_back(&AS_GR_GR_ASSIGN);
    assignmentOperators.push_back(&AS_LS_LS_LS_ASSIGN);
    assignmentOperators.push_back(&AS_LS_LS_ASSIGN);

    assignmentOperators.push_back(&AS_RETURN);

    nonAssignmentOperators.push_back(&AS_EQUAL);
    nonAssignmentOperators.push_back(&AS_PLUS_PLUS);
    nonAssignmentOperators.push_back(&AS_MINUS_MINUS);
    nonAssignmentOperators.push_back(&AS_NOT_EQUAL);
    nonAssignmentOperators.push_back(&AS_GR_EQUAL);
    nonAssignmentOperators.push_back(&AS_GR_GR_GR);
    nonAssignmentOperators.push_back(&AS_GR_GR);
    nonAssignmentOperators.push_back(&AS_LS_EQUAL);
    nonAssignmentOperators.push_back(&AS_LS_LS_LS);
    nonAssignmentOperators.push_back(&AS_LS_LS);
    nonAssignmentOperators.push_back(&AS_ARROW);
    nonAssignmentOperators.push_back(&AS_AND);
    nonAssignmentOperators.push_back(&AS_OR);
  }

  /**
   * ASBeautifier's constructor
   */
  ASBeautifier::ASBeautifier()
  {
    initStatic();

    waitingBeautifierStack = NULL;
    activeBeautifierStack = NULL;
    waitingBeautifierStackLengthStack = NULL;
    activeBeautifierStackLengthStack = NULL;

    headerStack  = NULL;
    tempStacks = NULL;
    blockParenDepthStack = NULL;
    blockStatementStack = NULL;
    parenStatementStack = NULL;
    bracketBlockStateStack = NULL;
    inStatementIndentStack = NULL;
    inStatementIndentStackSizeStack = NULL;
    parenIndentStack = NULL;
    sourceIterator = NULL;

    isMinimalConditinalIndentSet = false;
    shouldForceTabIndentation = false;

    setSpaceIndentation(4);
    setMaxInStatementIndentLength(40);
    setClassIndent(false);
    setSwitchIndent(false);
    setCaseIndent(false);
    setBlockIndent(false);
    setBracketIndent(false);
    setNamespaceIndent(false);
    setLabelIndent(false);
    setEmptyLineFill(false);
    setCStyle();
    setPreprocessorIndent(false);
  }

  ASBeautifier::ASBeautifier(const ASBeautifier &other)
  {
    waitingBeautifierStack = NULL;
    activeBeautifierStack = NULL;
    waitingBeautifierStackLengthStack = NULL;
    activeBeautifierStackLengthStack = NULL;

    headerStack  = new vector<const string*>;
    *headerStack = *other.headerStack;

    tempStacks = new vector< vector<const string*>* >;
    vector< vector<const string*>* >::iterator iter;
    for (iter = other.tempStacks->begin();
         iter != other.tempStacks->end();
         ++iter)
      {
        vector<const string*> *newVec = new vector<const string*>;
        *newVec = **iter;
        tempStacks->push_back(newVec);
      }
    blockParenDepthStack = new vector<int>;
    *blockParenDepthStack = *other.blockParenDepthStack;

    blockStatementStack = new vector<bool>;
    *blockStatementStack = *other.blockStatementStack;

    parenStatementStack =  new vector<bool>;
    *parenStatementStack = *other.parenStatementStack;

    bracketBlockStateStack = new vector<bool>;
    *bracketBlockStateStack = *other.bracketBlockStateStack;

    inStatementIndentStack = new vector<int>;
    *inStatementIndentStack = *other.inStatementIndentStack;

    inStatementIndentStackSizeStack = new vector<int>;
    *inStatementIndentStackSizeStack = *other.inStatementIndentStackSizeStack;

    parenIndentStack = new vector<int>;
    *parenIndentStack = *other.parenIndentStack;

    sourceIterator = other.sourceIterator;

    indentString = other.indentString;
    currentHeader = other.currentHeader;
    previousLastLineHeader = other.previousLastLineHeader;
    immediatelyPreviousAssignmentOp = other.immediatelyPreviousAssignmentOp;
    isInQuote = other.isInQuote;
    isInComment = other.isInComment;
    isInCase = other.isInCase;
    isInQuestion = other.isInQuestion;
    isInStatement =other. isInStatement;
    isInHeader = other.isInHeader;
    isCStyle = other.isCStyle;
    isInOperator = other.isInOperator;
    isInTemplate = other.isInTemplate;
    isInConst = other.isInConst;
    classIndent = other.classIndent;
    isInClassHeader = other.isInClassHeader;
    isInClassHeaderTab = other.isInClassHeaderTab;
    switchIndent = other.switchIndent;
    caseIndent = other.caseIndent;
    namespaceIndent = other.namespaceIndent;
    bracketIndent = other.bracketIndent;
    blockIndent = other.blockIndent;
    labelIndent = other.labelIndent;
    preprocessorIndent = other.preprocessorIndent;
    parenDepth = other.parenDepth;
    indentLength = other.indentLength;
    blockTabCount = other.blockTabCount;
    leadingWhiteSpaces = other.leadingWhiteSpaces;
    maxInStatementIndent = other.maxInStatementIndent;
    templateDepth = other.templateDepth;
    quoteChar = other.quoteChar;
    prevNonSpaceCh = other.prevNonSpaceCh;
    currentNonSpaceCh = other.currentNonSpaceCh;
    currentNonLegalCh = other.currentNonLegalCh;
    prevNonLegalCh = other.prevNonLegalCh;
    isInConditional = other.isInConditional;
    minConditionalIndent = other.minConditionalIndent;
    prevFinalLineSpaceTabCount = other.prevFinalLineSpaceTabCount;
    prevFinalLineTabCount = other.prevFinalLineTabCount;
    emptyLineFill = other.emptyLineFill;
    probationHeader = other.probationHeader;
    isInDefine = other.isInDefine;
    isInDefineDefinition = other.isInDefineDefinition;
    backslashEndsPrevLine = other.backslashEndsPrevLine;
    defineTabCount = other.defineTabCount;
  }

  /**
   * ASBeautifier's destructor
   */
  ASBeautifier::~ASBeautifier()
  {
    DELETE_CONTAINER( headerStack );
    DELETE_CONTAINER( tempStacks );
    DELETE_CONTAINER( blockParenDepthStack );
    DELETE_CONTAINER( blockStatementStack );
    DELETE_CONTAINER( parenStatementStack );
    DELETE_CONTAINER( bracketBlockStateStack );
    DELETE_CONTAINER( inStatementIndentStack );
    DELETE_CONTAINER( inStatementIndentStackSizeStack );
    DELETE_CONTAINER( parenIndentStack );

    // DELETE_CONTAINER( sourceIterator );
  }

  /**
   * initialize the ASBeautifier.
   *
   * init() should be called every time a ABeautifier object is to start
   * beautifying a NEW source file.
   * init() recieves a pointer to a DYNAMICALLY CREATED ASSourceIterator object
   * that will be used to iterate through the source code. This object will be
   * deleted during the ASBeautifier's destruction, and thus should not be
   * deleted elsewhere.
   *
   * @param iter     a pointer to the DYNAMICALLY CREATED ASSourceIterator object.
   */
  void ASBeautifier::init(ASSourceIterator *iter)

  {
    sourceIterator = iter;
    init();
  }

  /**
   * initialize the ASBeautifier.
   */
  void ASBeautifier::init()
  {
    INIT_CONTAINER( waitingBeautifierStack,  new vector<ASBeautifier*> );
    INIT_CONTAINER( activeBeautifierStack,  new vector<ASBeautifier*> );

    INIT_CONTAINER( waitingBeautifierStackLengthStack, new vector<int> );
    INIT_CONTAINER( activeBeautifierStackLengthStack, new vector<int> );

    INIT_CONTAINER( headerStack,  new vector<const string*> );
    INIT_CONTAINER( tempStacks, new vector< vector<const string*>* > );
    tempStacks->push_back(new vector<const string*>);

    INIT_CONTAINER( blockParenDepthStack, new vector<int> );
    INIT_CONTAINER( blockStatementStack, new vector<bool> );
    INIT_CONTAINER( parenStatementStack, new vector<bool> );

    INIT_CONTAINER( bracketBlockStateStack, new vector<bool> );
    bracketBlockStateStack->push_back(true);

    INIT_CONTAINER( inStatementIndentStack, new vector<int> );
    INIT_CONTAINER( inStatementIndentStackSizeStack, new vector<int> );
    inStatementIndentStackSizeStack->push_back(0);
    INIT_CONTAINER( parenIndentStack, new vector<int> );

    immediatelyPreviousAssignmentOp = NULL;
    previousLastLineHeader = NULL;

    isInQuote = false;
    isInComment = false;
    isInStatement = false;
    isInCase = false;
    isInQuestion = false;
    isInClassHeader = false;
    isInClassHeaderTab = false;
    isInHeader = false;
    isInOperator = false;
    isInTemplate = false;
    isInConst = false;
    isInConditional = false;
    templateDepth = 0;
    parenDepth=0;
    blockTabCount = 0;
    leadingWhiteSpaces = 0;
    prevNonSpaceCh = '{';
    currentNonSpaceCh = '{';
    prevNonLegalCh = '{';
    currentNonLegalCh = '{';
    prevFinalLineSpaceTabCount = 0;
    prevFinalLineTabCount = 0;
    probationHeader = NULL;
    backslashEndsPrevLine = false;
    isInDefine = false;
    isInDefineDefinition = false;
    defineTabCount = 0;
  }

  /**
   * set indentation style to ANSI C/C++.  
   */
  void ASBeautifier::setCStyle()
  {
    isCStyle = true;
  }

  /**
   * set indentation style to Java / K&R.  
   */
  void ASBeautifier::setJavaStyle()
  {
    isCStyle = false;
  }

  /**
   * indent using one tab per indentation
   */
  void ASBeautifier::setTabIndentation(int length, bool forceTabs)
  {
    indentString = "\t";
    indentLength = length;
    shouldForceTabIndentation = forceTabs;

    if (!isMinimalConditinalIndentSet)
      minConditionalIndent = indentLength * 2;
  }

  /**
   
   * indent using a number of spaces per indentation.
   *
   * @param   length     number of spaces per indent.
   */
  void ASBeautifier::setSpaceIndentation(int length)
  {
    indentString=string(length, ' ');
    indentLength = length;

    if (!isMinimalConditinalIndentSet)
      minConditionalIndent = indentLength * 2;
  }

  /**
   * set the maximum indentation between two lines in a multi-line statement.
   *
   * @param   max     maximum indentation length.
   */
  void ASBeautifier::setMaxInStatementIndentLength(int max)
  {
    maxInStatementIndent = max;
  }

  /**
   * set the minimum indentation between two lines in a multi-line condition.
   *
   * @param   min     minimal indentation length.
   */
  void ASBeautifier::setMinConditionalIndentLength(int min)
  {
    minConditionalIndent = min;
    isMinimalConditinalIndentSet = true;
  }

  /**
   * set the state of the bracket indentation option. If true, brackets will 
   * be indented one additional indent.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setBracketIndent(bool state)
  {
    bracketIndent = state;
  }

  /**
   * set the state of the block indentation option. If true, entire blocks 
   * will be indented one additional indent, similar to the GNU indent style.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setBlockIndent(bool state)
  {
    if (state)
      setBracketIndent(false); // so that we don't have both bracket and block indent
    blockIndent = state;
  }

  /**
   * set the state of the class indentation option. If true, C++ class
   * definitions will be indented one additional indent.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setClassIndent(bool state)
  {
    classIndent = state;
  }

  /**
   * set the state of the switch indentation option. If true, blocks of 'switch' 
   * statements will be indented one additional indent.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setSwitchIndent(bool state)
  {
    switchIndent = state;
  }

  /**
   * set the state of the case indentation option. If true, lines of 'case' 
   * statements will be indented one additional indent.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setCaseIndent(bool state)
  {
    caseIndent = state;
  }
  /**
   * set the state of the namespace indentation option. 
   * If true, blocks of 'namespace' statements will be indented one 
   * additional indent. Otherwise, NO indentation will be added.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setNamespaceIndent(bool state)
  {
    namespaceIndent = state;
  }

  /**
   * set the state of the label indentation option. 
   * If true, labels will be indented one indent LESS than the
   * current indentation level.
   * If false, labels will be flushed to the left with NO
   * indent at all.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setLabelIndent(bool state)
  {
    labelIndent = state;
  }

  /**
   * set the state of the preprocessor indentation option. 
   * If true, multiline #define statements will be indented.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setPreprocessorIndent(bool state)
  {
    preprocessorIndent = state;
  }

  /**
   * set the state of the empty line fill option. 
   * If true, empty lines will be filled with the whitespace.
   * of their previous lines.
   * If false, these lines will remain empty.
   *
   * @param   state             state of option.
   */
  void ASBeautifier::setEmptyLineFill(bool state)
  {
    emptyLineFill = state;
  }

  /**
   * check if there are any indented lines ready to be read by nextLine()
   *
   * @return    are there any indented lines ready?
   */
  bool ASBeautifier::hasMoreLines() const
    {
      return sourceIterator->hasMoreLines();
    }

  /**
   * get the next indented line.
   *
   * @return    indented line.
   */
  string ASBeautifier::nextLine()
  {
    return beautify(sourceIterator->nextLine());
  }

  /**
   * beautify a line of source code.
   * every line of source code in a source code file should be sent
   * one after the other to the beautify method.
   *
   * @return      the indented line.
   * @param originalLine       the original unindented line.
   */
  string ASBeautifier::beautify(const string &originalLine)
  {
    string line;
    bool isInLineComment = false;
    bool lineStartsInComment = false;
    bool isInClass = false;
    bool isInSwitch = false;
    bool isImmediatelyAfterConst = false;
    bool isSpecialChar = false;

    char ch = ' ';
    char prevCh;
    string outBuffer; // the newly idented line is bufferd here
    int tabCount = 0;
    const string *lastLineHeader = NULL;
    bool closingBracketReached = false;
    int spaceTabCount = 0;
    char tempCh;
    unsigned int headerStackSize = headerStack->size();
    //bool isLineInStatement = isInStatement;
    bool shouldIndentBrackettedLine = true;
    int lineOpeningBlocksNum = 0;
    int lineClosingBlocksNum = 0;
    bool previousLineProbation = (probationHeader != NULL);
    unsigned int i;

    currentHeader = NULL;

    lineStartsInComment = isInComment;

    // handle and remove white spaces around the line:
    // If not in comment, first find out size of white space before line,
    // so that possible comments starting in the line continue in
    // relation to the preliminary white-space.
    if (!isInComment)
      {
        leadingWhiteSpaces = 0;
        while (leadingWhiteSpaces<originalLine.length() && originalLine[leadingWhiteSpaces] <= 0x20)
          leadingWhiteSpaces++;

        line = trim(originalLine);
      }
    else
      {
        unsigned int trimSize;
        for (trimSize=0;
             trimSize < originalLine.length() && trimSize<leadingWhiteSpaces && originalLine[trimSize] <= 0x20 ;
             trimSize++)
          ;
        line = originalLine.substr(trimSize);
      }


    if (line.length() == 0)
      {
        if (emptyLineFill)
          return preLineWS(prevFinalLineSpaceTabCount, prevFinalLineTabCount);
        else
          return line;
      }

    // handle preprocessor commands

    if (isCStyle && !isInComment && (line[0] == '#' || backslashEndsPrevLine))
      {
        if (line[0] == '#')
          {
            string preproc = trim(string(line.c_str() + 1));


            // When finding a multi-lined #define statement, the original beautifier
            // 1. sets its isInDefineDefinition flag
            // 2. clones a new beautifier that will be used for the actual indentation
            //    of the #define. This clone is put into the activeBeautifierStack in order
            //    to be called for the actual indentation.
            // The original beautifier will have isInDefineDefinition = true, isInDefine = false
            // The cloned beautifier will have   isInDefineDefinition = true, isInDefine = true
            if (preprocessorIndent && preproc.COMPARE(0, 6, string("define")) == 0 &&  line[line.length() - 1] == '\\')
              {
                if (!isInDefineDefinition)
                  {
                    ASBeautifier *defineBeautifier;

                    // this is the original beautifier
                    isInDefineDefinition = true;

                    // push a new beautifier into the active stack
                    // this breautifier will be used for the indentation of this define
                    defineBeautifier = new ASBeautifier(*this);
                    //defineBeautifier->init();
                    //defineBeautifier->isInDefineDefinition = true;
                    //defineBeautifier->beautify("");
                    activeBeautifierStack->push_back(defineBeautifier);
                  }
                else
                  {
                    // the is the cloned beautifier that is in charge of indenting the #define.
                    isInDefine = true;
                  }
              }
            else if (preproc.COMPARE(0, 2, string("if")) == 0)
              {
                // push a new beautifier into the stack
                waitingBeautifierStackLengthStack->push_back(waitingBeautifierStack->size());
                activeBeautifierStackLengthStack->push_back(activeBeautifierStack->size());
                waitingBeautifierStack->push_back(new ASBeautifier(*this));
              }
            else if (preproc.COMPARE(0, 4/*2*/, string("else")) == 0)
              {
                if (!waitingBeautifierStack->empty())
                  {
                    // MOVE current waiting beautifier to active stack.
                    activeBeautifierStack->push_back(waitingBeautifierStack->back());
                    waitingBeautifierStack->pop_back();
                  }
              }
            else if (preproc.COMPARE(0, 4, string("elif")) == 0)
              {
                if (!waitingBeautifierStack->empty())
                  {
                    // append a COPY current waiting beautifier to active stack, WITHOUT deleting the original.
                    activeBeautifierStack->push_back( new ASBeautifier( *(waitingBeautifierStack->back()) ) );
                  }
              }
            else if (preproc.COMPARE(0, 5, string("endif")) == 0)
              {
                unsigned int stackLength;
                ASBeautifier *beautifier;

                if (!waitingBeautifierStackLengthStack->empty())
                  {
                    stackLength = waitingBeautifierStackLengthStack->back();
                    waitingBeautifierStackLengthStack->pop_back();
                    while (waitingBeautifierStack->size() > stackLength)
                      {
                        beautifier = waitingBeautifierStack->back();
                        waitingBeautifierStack->pop_back();
                        delete beautifier;
                      }
                  }

                if (!activeBeautifierStackLengthStack->empty())
                  {
                    stackLength = activeBeautifierStackLengthStack->back();
                    activeBeautifierStackLengthStack->pop_back();
                    while (activeBeautifierStack->size() > stackLength)
                      {
                        beautifier = activeBeautifierStack->back();
                        activeBeautifierStack->pop_back();
                        delete beautifier;
                      }
                  }


              }
          }

        // check if the last char is a backslash
        if(line.length() > 0)
          backslashEndsPrevLine = (line[line.length() - 1] == '\\');
        else
          backslashEndsPrevLine = false;

        // check if this line ends a multi-line #define
        // if so, use the #define's cloned beautifier for the line's indentation
        // and then remove it from the active beautifier stack and delete it.
        if (!backslashEndsPrevLine && isInDefineDefinition && !isInDefine)
          {
            string beautifiedLine;
            ASBeautifier *defineBeautifier;

            isInDefineDefinition = false;
            defineBeautifier = activeBeautifierStack->back();
            activeBeautifierStack->pop_back();

            beautifiedLine = defineBeautifier->beautify(line);
            delete defineBeautifier;
            return beautifiedLine;
          }

        // unless this is a multi-line #define, return this precompiler line as is.
        if (!isInDefine && !isInDefineDefinition)
          return originalLine;
      }

    // if there exists any worker beautifier in the activeBeautifierStack,
    // then use it instead of me to indent the current line.
    if (!isInDefine && activeBeautifierStack != NULL && !activeBeautifierStack->empty())
      {
        return activeBeautifierStack->back()->beautify(line);
      }

    // calculate preliminary indentation based on data from past lines
    if (!inStatementIndentStack->empty())
      spaceTabCount = inStatementIndentStack->back();


    for (i=0; i<headerStackSize; i++)
      {
        isInClass = false;

        if (blockIndent || (!(i>0 && (*headerStack)[i-1] != &AS_OPEN_BRACKET
                              && (*headerStack)[i] == &AS_OPEN_BRACKET)))
          ++tabCount;

        if (isCStyle && !namespaceIndent && i >= 1
            && (*headerStack)[i-1] == &AS_NAMESPACE
            && (*headerStack)[i] == &AS_OPEN_BRACKET)
          --tabCount;

        if (isCStyle && i >= 1
            && (*headerStack)[i-1] == &AS_CLASS
            && (*headerStack)[i] == &AS_OPEN_BRACKET )
          {
            if (classIndent)
              ++tabCount;
            isInClass = true;
          }

        // is the switchIndent option is on, indent switch statements an additional indent.
        else if (switchIndent && i > 1 &&
                 (*headerStack)[i-1] == &AS_SWITCH &&
                 (*headerStack)[i] == &AS_OPEN_BRACKET
                )
          {
            ++tabCount;
            isInSwitch = true;
          }

      }

    if (!lineStartsInComment
        && isCStyle
        && isInClass
        && classIndent
        && headerStackSize >= 2
        &&(*headerStack)[headerStackSize-2] == &AS_CLASS
        && (*headerStack)[headerStackSize-1] == &AS_OPEN_BRACKET
        && line[0] == '}')
      --tabCount;

    else if (!lineStartsInComment
             && isInSwitch
             && switchIndent
             && headerStackSize >= 2
             && (*headerStack)[headerStackSize-2] == &AS_SWITCH
             && (*headerStack)[headerStackSize-1] == &AS_OPEN_BRACKET
             && line[0] == '}')
      --tabCount;

    if (isInClassHeader)
      {
        isInClassHeaderTab = true;
        tabCount += 2;
      }

    if (isInConditional)
      {
        --tabCount;
      }


    // parse characters in the current line.

    for (i=0; i<line.length(); i++)
      {
        tempCh = line[i];

        prevCh = ch;
        ch = tempCh;

        outBuffer.append(1, ch);

        if (isWhiteSpace(ch))
          continue;


        // handle special characters (i.e. backslash+character such as \n, \t, ...)
        if (isSpecialChar)
          {
            isSpecialChar = false;
            continue;
          }
        if (!(isInComment || isInLineComment) && line.COMPARE(i, 2, string("\\\\")) == 0)
          {
            outBuffer.append(1, '\\');
            i++;
            continue;
          }
        if (!(isInComment || isInLineComment) && ch=='\\')
          {
            isSpecialChar = true;
            continue;
          }

        // handle quotes (such as 'x' and "Hello Dolly")
        if (!(isInComment || isInLineComment) && (ch=='"' || ch=='\''))
          if (!isInQuote)
            {
              quoteChar = ch;
              isInQuote = true;
            }
          else if (quoteChar == ch)
            {
              isInQuote = false;
              isInStatement = true;
              continue;
            }
        if (isInQuote)
          continue;

        // handle comments

        if ( !(isInComment || isInLineComment) && line.COMPARE(i, 2, AS_OPEN_LINE_COMMENT) == 0 )
          {
            isInLineComment = true;
            outBuffer.append(1, '/');
            i++;
            continue;
          }
        else if ( !(isInComment || isInLineComment) && line.COMPARE(i, 2, AS_OPEN_COMMENT) == 0 )
          {
            isInComment = true;
            outBuffer.append(1, '*');
            i++;
            continue;
          }
        else if ( (isInComment || isInLineComment) && line.COMPARE(i, 2, AS_CLOSE_COMMENT) == 0 )
          {
            isInComment = false;
            outBuffer.append(1, '/');
            i++;
            continue;
          }

        if (isInComment||isInLineComment)
          continue;

        // if we have reached this far then we are NOT in a comment or string of special character...

        if (probationHeader != NULL)
          {
            if ( ((probationHeader == &AS_STATIC || probationHeader == &AS_CONST) && ch == '{')
                 || (probationHeader == &AS_SYNCHRONIZED && ch == '('))
              {
                // insert the probation header as a new header
                isInHeader = true;
                headerStack->push_back(probationHeader);

                // handle the specific probation header
                isInConditional = (probationHeader == &AS_SYNCHRONIZED);
                if (probationHeader == &AS_CONST)
                  isImmediatelyAfterConst = true;
                //  isInConst = true;
                /* TODO:
                 * There is actually no more need for the global isInConst variable.
                               * The only reason for checking const is to see if there is a const
                 * immediately before an open-bracket.
                 * Since CONST is now put into probation and is checked during itspost-char,
                 * isImmediatelyAfterConst can be set by its own...
                 */

                isInStatement = false;
                // if the probation comes from the previous line, then indent by 1 tab count.
                if (previousLineProbation && ch == '{')
                  tabCount++;
                previousLineProbation = false;
              }

            // dismiss the probation header
            probationHeader = NULL;
          }

        prevNonSpaceCh = currentNonSpaceCh;
        currentNonSpaceCh = ch;
        if (!isLegalNameChar(ch) && ch != ',' && ch != ';' )
          {
            prevNonLegalCh = currentNonLegalCh;
            currentNonLegalCh = ch;
          }

        //if (isInConst)
        //{
        //    isInConst = false;
        //    isImmediatelyAfterConst = true;
        //}

        if (isInHeader)
          {
            isInHeader = false;
            currentHeader = headerStack->back();
          }
        else
          currentHeader = NULL;

        if (isCStyle && isInTemplate
            && (ch == '<' || ch == '>')
            &&  findHeader(line, i, nonAssignmentOperators) == NULL) //;
          {
            if (ch == '<')
              {
                ++templateDepth;
              }
            else if (ch == '>')
              {
                if (--templateDepth <= 0)
                  {
                    if (isInTemplate)
                      ch = ';';
                    else
                      ch = 't';
                    isInTemplate = false;
                    templateDepth = 0;
                  }

              }
          }

        // handle parenthesies
        if (ch == '(' || ch == '[' || ch == ')' || ch == ']')
          {
            if (ch == '(' || ch == '[')
              {
                if (parenDepth == 0)
                  {
                    parenStatementStack->push_back(isInStatement);
                    isInStatement = true;
                  }
                parenDepth++;

                inStatementIndentStackSizeStack->push_back(inStatementIndentStack->size());

                if (currentHeader != NULL)
                  registerInStatementIndent(line, i, spaceTabCount, minConditionalIndent/*indentLength*2*/, true);
                else
                  registerInStatementIndent(line, i, spaceTabCount, 0, true);
              }
            else if (ch == ')' || ch == ']')
              {
                parenDepth--;
                if (parenDepth == 0)
                  {
                    isInStatement = parenStatementStack->back();
                    parenStatementStack->pop_back();
                    ch = ' ';

                    isInConditional = false;
                  }

                if (!inStatementIndentStackSizeStack->empty())
                  {
                    unsigned int previousIndentStackSize = inStatementIndentStackSizeStack->back();
                    inStatementIndentStackSizeStack->pop_back();
                    while (previousIndentStackSize < inStatementIndentStack->size())
                      inStatementIndentStack->pop_back();

                    if (!parenIndentStack->empty())
                      {
                        int poppedIndent = parenIndentStack->back();
                        parenIndentStack->pop_back();

                        if (i == 0)
                          spaceTabCount = poppedIndent;
                      }
                  }
              }

            continue;
          }


        if (ch == '{')
          {
            bool isBlockOpener = false;

            // first, check if '{' is a block-opener or an static-array opener
            isBlockOpener = ( (prevNonSpaceCh == '{' && bracketBlockStateStack->back())
                              || prevNonSpaceCh == '}'
                              || prevNonSpaceCh == ')'
                              || prevNonSpaceCh == ';'
                              || isInClassHeader
                              || isBlockOpener
                              || isImmediatelyAfterConst
                              || (isInDefine &&
                                  (prevNonSpaceCh == '('
                                   || prevNonSpaceCh == '_'
                                   || isalnum(prevNonSpaceCh))) );

            isInClassHeader = false;
            if (!isBlockOpener && currentHeader != NULL)
              {
                for (unsigned int n=0; n < nonParenHeaders.size(); n++)
                  if (currentHeader == nonParenHeaders[n])
                    {
                      isBlockOpener = true;
                      break;
                    }
              }
            bracketBlockStateStack->push_back(isBlockOpener);
            if (!isBlockOpener)
              {
                inStatementIndentStackSizeStack->push_back(inStatementIndentStack->size());
                registerInStatementIndent(line, i, spaceTabCount, 0, true);
                parenDepth++;
                if (i == 0)
                  shouldIndentBrackettedLine = false;

                continue;
              }

            // this bracket is a block opener...

            ++lineOpeningBlocksNum;

            if (isInClassHeader)
              isInClassHeader = false;
            if (isInClassHeaderTab)
              {
                isInClassHeaderTab = false;
                tabCount -= 2;
              }

            blockParenDepthStack->push_back(parenDepth);
            blockStatementStack->push_back(isInStatement);

            inStatementIndentStackSizeStack->push_back(inStatementIndentStack->size());

            blockTabCount += isInStatement? 1 : 0;
            parenDepth = 0;
            isInStatement = false;

            tempStacks->push_back(new vector<const string*>);
            headerStack->push_back(&AS_OPEN_BRACKET);
            lastLineHeader = &AS_OPEN_BRACKET; // <------

            continue;
          }

        //check if a header has been reached
        if (prevCh == ' ')
          {
            bool isIndentableHeader = true;
            const string *newHeader = findHeader(line, i, headers);
            if (newHeader != NULL)
              {
                // if we reached here, then this is a header...
                isInHeader = true;

                vector<const string*> *lastTempStack;
                if (tempStacks->empty())
                  lastTempStack = NULL;
                else
                  lastTempStack = tempStacks->back();

                // if a new block is opened, push a new stack into tempStacks to hold the
                // future list of headers in the new block.

                // take care of the special case: 'else if (...)'
                if (newHeader == &AS_IF && lastLineHeader == &AS_ELSE)
                  {
                    //spaceTabCount += indentLength; // to counter the opposite addition that occurs when the 'if' is registered below...
                    headerStack->pop_back();
                  }

                // take care of 'else'
                else if (newHeader == &AS_ELSE)
                  {
                    if (lastTempStack != NULL)
                      {
                        int indexOfIf = indexOf(*lastTempStack, &AS_IF); // <---
                        if (indexOfIf != -1)
                          {
                            // recreate the header list in headerStack up to the previous 'if'
                            // from the temporary snapshot stored in lastTempStack.
                            int restackSize = lastTempStack->size() - indexOfIf - 1;
                            for (int r=0; r<restackSize; r++)
                              {
                                headerStack->push_back(lastTempStack->back());
                                lastTempStack->pop_back();
                              }
                            if (!closingBracketReached)
                              tabCount += restackSize;
                          }
                        /*
                         * If the above if is not true, i.e. no 'if' before the 'else',
                         * then nothing beautiful will come out of this...
                         * I should think about inserting an Exception here to notify the caller of this...
                         */
                      }
                  }

                // check if 'while' closes a previous 'do'
                else if (newHeader == &AS_WHILE)
                  {
                    if (lastTempStack != NULL)
                      {
                        int indexOfDo = indexOf(*lastTempStack, &AS_DO); // <---
                        if (indexOfDo != -1)
                          {
                            // recreate the header list in headerStack up to the previous 'do'
                            // from the temporary snapshot stored in lastTempStack.
                            int restackSize = lastTempStack->size() - indexOfDo - 1;
                            for (int r=0; r<restackSize; r++)
                              {
                                headerStack->push_back(lastTempStack->back());
                                lastTempStack->pop_back();
                              }
                            if (!closingBracketReached)
                              tabCount += restackSize;
                          }
                      }
                  }
                // check if 'catch' closes a previous 'try' or 'catch'
                else if (newHeader == &AS_CATCH || newHeader == &AS_FINALLY)
                  {
                    if (lastTempStack != NULL)
                      {
                        int indexOfTry = indexOf(*lastTempStack, &AS_TRY);
                        if (indexOfTry == -1)
                          indexOfTry = indexOf(*lastTempStack, &AS_CATCH);
                        if (indexOfTry != -1)
                          {
                            // recreate the header list in headerStack up to the previous 'try'
                            // from the temporary snapshot stored in lastTempStack.
                            int restackSize = lastTempStack->size() - indexOfTry - 1;
                            for (int r=0; r<restackSize; r++)
                              {
                                headerStack->push_back(lastTempStack->back());
                                lastTempStack->pop_back();
                              }

                            if (!closingBracketReached)
                              tabCount += restackSize;
                          }
                      }
                  }
                else if (newHeader == &AS_CASE)
                  {
                    isInCase = true;
                    if (!caseIndent)
                      --tabCount;
                  }
                else if(newHeader == &AS_DEFAULT)
                  {
                    isInCase = true;
                    if (!caseIndent)
                      --tabCount;
                  }
                else if (newHeader == &AS_PUBLIC || newHeader == &AS_PROTECTED || newHeader == &AS_PRIVATE)
                  {
                    if (isCStyle && !isInClassHeader)
                      --tabCount;
                    isIndentableHeader = false;
                  }
                //else if ((newHeader == &STATIC || newHeader == &SYNCHRONIZED) &&
                //         !headerStack->empty() &&
                //         (headerStack->back() == &STATIC || headerStack->back() == &SYNCHRONIZED))
                //{
                //    isIndentableHeader = false;
                //}
                else if (newHeader == &AS_STATIC
                         || newHeader == &AS_SYNCHRONIZED
                         || (newHeader == &AS_CONST && isCStyle))
                  {
                    if (!headerStack->empty() &&
                        (headerStack->back() == &AS_STATIC
                         || headerStack->back() == &AS_SYNCHRONIZED
                         || headerStack->back() == &AS_CONST))
                      {
                        isIndentableHeader = false;
                      }
                    else
                      {
                        isIndentableHeader = false;
                        probationHeader = newHeader;
                      }
                  }
                else if (newHeader == &AS_CONST)
                  {
                    // this will be entered only if NOT in C style
                    // since otherwise the CONST would be found to be a probstion header...

                    //if (isCStyle)
                    //  isInConst = true;
                    isIndentableHeader = false;
                  }
                /*
                              else if (newHeader == &OPERATOR)
                              {
                                  if (isCStyle)
                                      isInOperator = true;
                                  isIndentableHeader = false;
                              }
                */
                else if (newHeader == &AS_TEMPLATE)
                  {
                    if (isCStyle)
                      isInTemplate = true;
                    isIndentableHeader = false;
                  }


                if (isIndentableHeader)
                  {
                    // 3.2.99
                    //spaceTabCount-=indentLength;
                    headerStack->push_back(newHeader);
                    isInStatement = false;
                    if (indexOf(nonParenHeaders, newHeader) == -1)
                      {
                        isInConditional = true;
                      }
                    lastLineHeader = newHeader;
                  }
                else
                  isInHeader = false;

                //lastLineHeader = newHeader;

                outBuffer.append(newHeader->substr(1));
                i += newHeader->length() - 1;

                continue;
              }
          }

        if (isCStyle && !isalpha(prevCh)
            && line.COMPARE(i, 8, AS_OPERATOR) == 0 && !isalnum(line[i+8]))
          {
            isInOperator = true;
            outBuffer.append(AS_OPERATOR.substr(1));
            i += 7;
            continue;
          }

        if (ch == '?')
          isInQuestion = true;


        // special handling of 'case' statements
        if (ch == ':')
          {
            if (line.length() > i+1 && line[i+1] == ':') // look for ::
              {
                ++i;
                outBuffer.append(1, ':');
                ch = ' ';
                continue;
              }

            else if (isCStyle && isInClass && prevNonSpaceCh != ')')
              {
              // BEGIN Content of ASBeautifier.cpp.BITFIELD.patch:
              
                unsigned int chIndex;
   			    char nextCh = 0;
                for (chIndex = i+1; chIndex < line.length(); chIndex++)
            		if (!isWhiteSpace(line[chIndex]))
						break;
					if (chIndex< line.length())
       					nextCh = line[chIndex];
				int nWord =0;
    			for (chIndex = 0; chIndex < i; chIndex++)
				{
					if (!isWhiteSpace(line[chIndex]))
					{
						nWord ++;
						while (!isWhiteSpace(line[++chIndex]));
					}									
				}
				if ((nextCh >= '0' && nextCh <= '9') || (nWord >1))
					continue;
              // END Content of ASBeautifier.cpp.BITFIELD.patch:
                
                --tabCount;
                // found a 'private:' or 'public:' inside a class definition
                // so do nothing special
              }

            else if (isCStyle && isInClassHeader)
              {

                // found a 'class A : public B' definition
                // so do nothing special
              }

            else if (isInQuestion)
              {
                isInQuestion = false;
              }
            else if (isCStyle && prevNonSpaceCh == ')')
              {
                isInClassHeader = true;
                if (i==0)
                  tabCount += 2;
              }
            else
              {
                currentNonSpaceCh = ';'; // so that brackets after the ':' will appear as block-openers
                if (isInCase)
                  {
                    isInCase = false;
                    ch = ';'; // from here on, treat char as ';'
                  } 
              // BEGIN content of ASBeautifier.cpp.BITFIELD.patch.bz2
              else // bitfield or labels
								{
				unsigned int chIndex;
				char nextCh = 0;
				for (chIndex = i+1; (isCStyle && chIndex < line.length()); chIndex++)
					if (!isWhiteSpace(line[chIndex]))
						break;
				if (chIndex< line.length())
					nextCh = line[chIndex];

     			int nWord =0;
 				for (chIndex = 0; chIndex < i; chIndex++)
				{
					if (!isWhiteSpace(line[chIndex]))
					{
						nWord ++;
						while (!isWhiteSpace(line[++chIndex]));
					}									
				}
         		if (isCStyle &&  (nextCh >= '0' && nextCh <= '9') || (nWord >1))
				{
					continue;
				}
                // END content of ASASBeautifier.cpp.BITFIELD.patch.bz2

                else // is in a label (e.g. 'label1:')
                  {
                    if (labelIndent)
                      --tabCount; // unindent label by one indent
                    else
                      tabCount = 0; // completely flush indent to left
                  }

              // BEGIN content of ASASBeautifier.cpp.BITFIELD.patch.bz2
                }
            // END content of ASASBeautifier.cpp.BITFIELD.patch.bz2

              }
          }

        if ((ch == ';'  || (parenDepth>0 && ch == ','))  && !inStatementIndentStackSizeStack->empty())
          while ((unsigned int)inStatementIndentStackSizeStack->back() + (parenDepth>0 ? 1 : 0)  < inStatementIndentStack->size())
            inStatementIndentStack->pop_back();


        // handle ends of statements
        if ( (ch == ';' && parenDepth == 0) || ch == '}'/* || (ch == ',' && parenDepth == 0)*/)
          {
            if (ch == '}')
              {
                // first check if this '}' closes a previous block, or a static array...
                if (!bracketBlockStateStack->empty())
                  {
                    bool bracketBlockState = bracketBlockStateStack->back();
                    bracketBlockStateStack->pop_back();
                    if (!bracketBlockState)
                      {
                        if (!inStatementIndentStackSizeStack->empty())
                          {
                            // this bracket is a static array

                            unsigned int previousIndentStackSize = inStatementIndentStackSizeStack->back();
                            inStatementIndentStackSizeStack->pop_back();
                            while (previousIndentStackSize < inStatementIndentStack->size())
                              inStatementIndentStack->pop_back();
                            parenDepth--;
                            if (i == 0)
                              shouldIndentBrackettedLine = false;

                            if (!parenIndentStack->empty())
                              {
                                int poppedIndent = parenIndentStack->back();
                                parenIndentStack->pop_back();
                                if (i == 0)
                                  spaceTabCount = poppedIndent;
                              }
                          }
                        continue;
                      }
                  }

                // this bracket is block closer...

                ++lineClosingBlocksNum;

                if(!inStatementIndentStackSizeStack->empty())
                  inStatementIndentStackSizeStack->pop_back();

                if (!blockParenDepthStack->empty())
                  {
                    parenDepth = blockParenDepthStack->back();
                    blockParenDepthStack->pop_back();
                    isInStatement = blockStatementStack->back();
                    blockStatementStack->pop_back();

                    if (isInStatement)
                      blockTabCount--;
                  }

                closingBracketReached = true;
                int headerPlace = indexOf(*headerStack, &AS_OPEN_BRACKET); // <---
                if (headerPlace != -1)
                  {
                    const string *popped = headerStack->back();
                    while (popped != &AS_OPEN_BRACKET)
                      {
                        headerStack->pop_back();
                        popped = headerStack->back();
                      }
                    headerStack->pop_back();

                    if (!tempStacks->empty())
                      {
                        vector<const string*> *temp =  tempStacks->back();
                        tempStacks->pop_back();
                        delete temp;
                      }
                  }


                ch = ' '; // needed due to cases such as '}else{', so that headers ('else' tn tih case) will be identified...
              }

            /*
             * Create a temporary snapshot of the current block's header-list in the
             * uppermost inner stack in tempStacks, and clear the headerStack up to
             * the begining of the block.
             * Thus, the next future statement will think it comes one indent past
             * the block's '{' unless it specifically checks for a companion-header
             * (such as a previous 'if' for an 'else' header) within the tempStacks,
             * and recreates the temporary snapshot by manipulating the tempStacks.
             */
            if (!tempStacks->back()->empty())
              while (!tempStacks->back()->empty())
                tempStacks->back()->pop_back();
            while (!headerStack->empty() && headerStack->back() != &AS_OPEN_BRACKET)
              {
                tempStacks->back()->push_back(headerStack->back());
                headerStack->pop_back();
              }

            if (parenDepth == 0 && ch == ';')
              isInStatement=false;

            isInClassHeader = false;

            continue;
          }


        // check for preBlockStatements ONLY if not within parenthesies
        // (otherwise 'struct XXX' statements would be wrongly interpreted...)
        if (prevCh == ' ' && !isInTemplate && parenDepth == 0)
          {
            const string *newHeader = findHeader(line, i, preBlockStatements);
            if (newHeader != NULL)
              {
                isInClassHeader = true;
                outBuffer.append(newHeader->substr(1));
                i += newHeader->length() - 1;
                //if (isCStyle)
                headerStack->push_back(newHeader);
              }
          }

        // Handle operators
        //

        ////        // PRECHECK if a '==' or '--' or '++' operator was reached.
        ////        // If not, then register an indent IF an assignment operator was reached.
        ////        // The precheck is important, so that statements such as 'i--==2' are not recognized
        ////        // to have assignment operators (here, '-=') in them . . .

        const string *foundAssignmentOp = NULL;
        const string *foundNonAssignmentOp = NULL;

        immediatelyPreviousAssignmentOp = NULL;

        // Check if an operator has been reached.
        foundAssignmentOp = findHeader(line, i, assignmentOperators, false);
        foundNonAssignmentOp = findHeader(line, i, nonAssignmentOperators, false);

        // Since findHeader's boundry checking was not used above, it is possible
        // that both an assignment op and a non-assignment op where found,
        // e.g. '>>' and '>>='. If this is the case, treat the LONGER one as the
        // found operator.
        if (foundAssignmentOp != NULL && foundNonAssignmentOp != NULL)
          if (foundAssignmentOp->length() < foundNonAssignmentOp->length())
            foundAssignmentOp = NULL;
          else
            foundNonAssignmentOp = NULL;

        if (foundNonAssignmentOp != NULL)
          {
            if (foundNonAssignmentOp->length() > 1)
              {
                outBuffer.append(foundNonAssignmentOp->substr(1));
                i += foundNonAssignmentOp->length() - 1;
              }
          }

        else if (foundAssignmentOp != NULL)

          {
            if (foundAssignmentOp->length() > 1)
              {
                outBuffer.append(foundAssignmentOp->substr(1));
                i += foundAssignmentOp->length() - 1;
              }

            if (!isInOperator && !isInTemplate)
              {
                registerInStatementIndent(line, i, spaceTabCount, 0, false);
                immediatelyPreviousAssignmentOp = foundAssignmentOp;
                isInStatement = true;
              }
          }

        /*
                immediatelyPreviousAssignmentOp = NULL;
                bool isNonAssingmentOperator = false;
                for (int n = 0; n < nonAssignmentOperators.size(); n++)
                    if (line.COMPARE(i, nonAssignmentOperators[n]->length(), *(nonAssignmentOperators[n])) == 0)
                    {
                        if (nonAssignmentOperators[n]->length() > 1)
                        {
                            outBuffer.append(nonAssignmentOperators[n]->substr(1));
                            i += nonAssignmentOperators[n]->length() - 1;
                        }
                        isNonAssingmentOperator = true;
                        break;
                    }
                if (!isNonAssingmentOperator)
                {
                    for (int a = 0; a < assignmentOperators.size(); a++)
                        if (line.COMPARE(i, assignmentOperators[a]->length(), *(assignmentOperators[a])) == 0)
                        {
                            if (assignmentOperators[a]->length() > 1)
                            {
                                outBuffer.append(assignmentOperators[a]->substr(1));
                                i += assignmentOperators[a]->length() - 1;
                            }
         
                            if (!isInOperator && !isInTemplate)
                            {
                                registerInStatementIndent(line, i, spaceTabCount, 0, false);
                                immediatelyPreviousAssignmentOp = assignmentOperators[a];
                                isInStatement = true;
                            }
                            break;
                        }
                }
        */

        if (isInOperator)
          isInOperator = false;
      }

    // handle special cases of unindentation:

    /*
     * if '{' doesn't follow an immediately previous '{' in the headerStack
     * (but rather another header such as "for" or "if", then unindent it
     * by one indentation relative to its block.
     */
    //    cerr << endl << lineOpeningBlocksNum << " " <<  lineClosingBlocksNum << " " <<  previousLastLineHeader << endl;

    // indent #define lines with one less tab
    //if (isInDefine)
    //    tabCount -= defineTabCount-1;


    if (!lineStartsInComment
        && !blockIndent
        && outBuffer.length()>0
        && outBuffer[0]=='{'
        && !(lineOpeningBlocksNum > 0 && lineOpeningBlocksNum == lineClosingBlocksNum)
        && !(headerStack->size() > 1 && (*headerStack)[headerStack->size()-2] == &AS_OPEN_BRACKET)
        && shouldIndentBrackettedLine)
      --tabCount;

    else if (!lineStartsInComment
             && outBuffer.length()>0
             && outBuffer[0]=='}'
             && shouldIndentBrackettedLine )
      --tabCount;

    // correctly indent one-line-blocks...
    else if (!lineStartsInComment
             && outBuffer.length()>0
             && lineOpeningBlocksNum > 0
             && lineOpeningBlocksNum == lineClosingBlocksNum
             && previousLastLineHeader != NULL
             && previousLastLineHeader != &AS_OPEN_BRACKET)
      tabCount -= 1; //lineOpeningBlocksNum - (blockIndent ? 1 : 0);

    if (tabCount < 0)
      tabCount = 0;

    // take care of extra bracket indentatation option...
    if (bracketIndent && outBuffer.length()>0 && shouldIndentBrackettedLine)
      if (outBuffer[0]=='{' || outBuffer[0]=='}')
        tabCount++;


    if (isInDefine)
      {
        if (outBuffer[0] == '#')
          {
            string preproc = trim(string(outBuffer.c_str() + 1));
            if (preproc.COMPARE(0, 6, string("define")) == 0)
              {
                if (!inStatementIndentStack->empty()
                    && inStatementIndentStack->back() > 0)
                  {
                    defineTabCount = tabCount;
                  }
                else
                  {
                    defineTabCount = tabCount - 1;
                    tabCount--;
                  }
              }
          }

        tabCount -= defineTabCount;
      }

    if (tabCount < 0)
      tabCount = 0;


    // finally, insert indentations into begining of line

    prevFinalLineSpaceTabCount = spaceTabCount;
    prevFinalLineTabCount = tabCount;

    if (shouldForceTabIndentation)
      {
        tabCount += spaceTabCount / indentLength;
        spaceTabCount = spaceTabCount % indentLength;
      }

    outBuffer = preLineWS(spaceTabCount,tabCount) + outBuffer;

    if (lastLineHeader != NULL)
      previousLastLineHeader = lastLineHeader;

    return outBuffer;
  }


  string ASBeautifier::preLineWS(int spaceTabCount, int tabCount)
  {
    string ws;

    for (int i=0; i<tabCount; i++)
      ws += indentString;

    while ((spaceTabCount--) > 0)
      ws += string(" ");

    return ws;

  }

  /**
   * register an in-statement indent.
   */
  void ASBeautifier::registerInStatementIndent(const string &line, int i, int spaceTabCount,
      int minIndent, bool updateParenStack)
  {
    int inStatementIndent;
    int remainingCharNum = line.length() - i;
    int nextNonWSChar = 1;

    nextNonWSChar = getNextProgramCharDistance(line, i);

    // if indent is around the last char in the line, indent instead 2 spaces from the previous indent
    if (nextNonWSChar == remainingCharNum)
      {
        int previousIndent = spaceTabCount;
        if (!inStatementIndentStack->empty())
          previousIndent = inStatementIndentStack->back();

        inStatementIndentStack->push_back(/*2*/ indentLength + previousIndent );
        if (updateParenStack)
          parenIndentStack->push_back( previousIndent );
        return;
      }

    if (updateParenStack)
      parenIndentStack->push_back(i+spaceTabCount);

    inStatementIndent = i + nextNonWSChar + spaceTabCount;

    if (i + nextNonWSChar < minIndent)
      inStatementIndent = minIndent + spaceTabCount;

    if (i + nextNonWSChar > maxInStatementIndent)
      inStatementIndent =  indentLength*2 + spaceTabCount;



    if (!inStatementIndentStack->empty() &&
        inStatementIndent < inStatementIndentStack->back())
      inStatementIndent = inStatementIndentStack->back();

    inStatementIndentStack->push_back(inStatementIndent);
  }

  /**
   * get distance to the next non-white sspace, non-comment character in the line.
   * if no such character exists, return the length remaining to the end of the line.
   */
  int ASBeautifier::getNextProgramCharDistance(const string &line, int i)
  {
    bool inComment = false;
    int remainingCharNum = line.length() - i;
    int charDistance = 1;
    int ch;

    for (charDistance = 1; charDistance < remainingCharNum; charDistance++)
      {
        ch = line[i + charDistance];
        if (inComment)
          {
            if (line.COMPARE(i + charDistance, 2, AS_CLOSE_COMMENT) == 0)
              {
                charDistance++;
                inComment = false;
              }
            continue;
          }
        else if (isWhiteSpace(ch))
          continue;
        else if (ch == '/')
          {
            if (line.COMPARE(i + charDistance, 2, AS_OPEN_LINE_COMMENT) == 0)
              return remainingCharNum;
            else if (line.COMPARE(i + charDistance, 2, AS_OPEN_COMMENT) == 0)
              {
                charDistance++;
                inComment = true;
              }
          }
        else
          return charDistance;
      }

    return charDistance;
  }


  /**
   * check if a specific character can be used in a legal variable/method/class name
   *
   * @return          legality of the char.
   * @param ch        the character to be checked.
   */
  bool ASBeautifier::isLegalNameChar(char ch) const
    {
      return (isalnum(ch) //(ch>='a' && ch<='z') || (ch>='A' && ch<='Z') || (ch>='0' && ch<='9') ||
              || ch=='.' || ch=='_' || (!isCStyle && ch=='$') || (isCStyle && ch=='~'));
    }


  /**
   * check if a specific line position contains a header, out of several possible headers.
   *
   * @return    a pointer to the found header. if no header was found then return NULL.
   */
  const string *ASBeautifier::findHeader(const string &line, int i, const vector<const string*> &possibleHeaders, bool checkBoundry)
  {
    int maxHeaders = possibleHeaders.size();
    const string *header = NULL;
    int p;

    for (p=0; p < maxHeaders; p++)
      {
        header = possibleHeaders[p];

        if (line.COMPARE(i, header->length(), *header) == 0)
          {
            // check that this is a header and not a part of a longer word
            // (e.g. not at its begining, not at its middle...)

            int lineLength = line.length();
            int headerEnd = i + header->length();
            char startCh = (*header)[0];   // first char of header
            char endCh = 0;                // char just after header
            char prevCh = 0;               // char just before header

            if (headerEnd < lineLength)
              {
                endCh = line[headerEnd];
              }
            if (i > 0)
              {
                prevCh = line[i-1];
              }

            if (!checkBoundry)
              {
                return header;
              }
            else if (prevCh != 0
                     && isLegalNameChar(startCh)
                     && isLegalNameChar(prevCh))
              {
                return NULL;
              }
            else if (headerEnd >= lineLength
                     || !isLegalNameChar(startCh)
                     || !isLegalNameChar(endCh))
              {
                return header;
              }
            else
              {
                return NULL;
              }
          }
      }

    return NULL;
  }


  /**
   * check if a specific character can be used in a legal variable/method/class name
   *
   * @return          legality of the char.
   * @param ch        the character to be checked.
   */
  bool ASBeautifier::isWhiteSpace(char ch) const
    {
      return (ch == ' ' || ch == '\t');
    }

  /**
   * find the index number of a string element in a container of strings
   *
   * @return              the index number of element in the ocntainer. -1 if element not found.
   * @param container     a vector of strings.
   * @param element       the element to find .
   */
  int ASBeautifier::indexOf(vector<const string*> &container, const string *element)
  {
    vector<const string*>::const_iterator where;

    where= find(container.begin(), container.end(), element);
    if (where == container.end())
      return -1;
    else
      return where - container.begin();
  }

  /**
   * trim removes the white space surrounding a line.
   *
   * @return          the trimmed line.
   * @param str       the line to trim.
   */
  string ASBeautifier::trim(const string &str)
  {

    int start = 0;
    int end = str.length() - 1;

    while (start < end && isWhiteSpace(str[start]))
      start++;

    while (start <= end && isWhiteSpace(str[end]))
      end--;

    string returnStr(str, start, end+1-start);
    return returnStr;
  }

#ifdef USES_NAMESPACE
}
#endif
/*
 * Copyright (c) 1998,1999,2000,2001,2002 Tal Davidson. All rights reserved.
 *
 * compiler_defines.h   (1 January 1999)
 * by Tal Davidson (davidsont@bigfoot.com)
 * This file is a part of "Artistic Style" - an indentater and reformatter
 * of C, C++, C# and Java source files.
 *
 * The "Artistic Style" project, including all files needed to compile it,
 * is free software; you can redistribute it and/or use it and/or modify it
 * under the terms of the GNU General Public License as published 
 * by the Free Software Foundation; either version 2 of the License, 
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.
 */


#ifndef ASBEAUTIFIER_H
#define ASBEAUTIFIER_H

#include "ASResource.h"
#include "compiler_defines.h"
#include "ASSourceIterator.h"

#include <string>
#include <vector>


using namespace std;

namespace astyle
  {

  enum BracketMode   { NONE_MODE, ATTACH_MODE, BREAK_MODE, BDAC_MODE };
  enum BracketType   { NULL_TYPE = 0,
                       DEFINITION_TYPE = 1,
                       COMMAND_TYPE = 2,
                       ARRAY_TYPE  = 4,
                       SINGLE_LINE_TYPE = 8};


  class ASBeautifier : protected ASResource
    {
    public:
      ASBeautifier();
      virtual ~ASBeautifier();
      virtual void init(ASSourceIterator* iter); // pointer to dynamically created iterator.
      virtual void init();
      virtual bool hasMoreLines() const;
      virtual string nextLine();
      virtual string beautify(const string &line);
      void setTabIndentation(int length = 4, bool forceTabs = false);
      void setSpaceIndentation(int length = 4);
      void setMaxInStatementIndentLength(int max);
      void setMinConditionalIndentLength(int min);
      void setClassIndent(bool state);
      void setSwitchIndent(bool state);
      void setCaseIndent(bool state);
      void setBracketIndent(bool state);
      void setBlockIndent(bool state);
      void setNamespaceIndent(bool state);
      void setLabelIndent(bool state);
      void setCStyle();
      void setJavaStyle();
      void setEmptyLineFill(bool state);
      void setPreprocessorIndent(bool state);


    protected:
      int getNextProgramCharDistance(const string &line, int i);
      bool isLegalNameChar(char ch) const;
      bool isWhiteSpace(char ch) const;
      const string *findHeader(const string &line, int i,
                               const vector<const string*> &possibleHeaders,
                               bool checkBoundry = true);
      string trim(const string &str);
      int indexOf(vector<const string*> &container, const string *element);

    private:
      ASBeautifier(const ASBeautifier &copy);
      void operator=(ASBeautifier&); // not to be implemented

      void initStatic();
      void registerInStatementIndent(const string &line, int i, int spaceTabCount,
                                     int minIndent, bool updateParenStack);
      string preLineWS(int spaceTabCount, int tabCount);

      static vector<const string*> headers;
      static vector<const string*> nonParenHeaders;
      static vector<const string*> preprocessorHeaders;
      static vector<const string*> preBlockStatements;
      static vector<const string*> assignmentOperators;
      static vector<const string*> nonAssignmentOperators;

      static bool calledInitStatic;

      ASSourceIterator *sourceIterator;
      vector<ASBeautifier*> *waitingBeautifierStack;
      vector<ASBeautifier*> *activeBeautifierStack;
      vector<int> *waitingBeautifierStackLengthStack;
      vector<int> *activeBeautifierStackLengthStack;
      vector<const string*> *headerStack;
      vector< vector<const string*>* > *tempStacks;
      vector<int> *blockParenDepthStack;
      vector<bool> *blockStatementStack;
      vector<bool> *parenStatementStack;
      vector<int> *inStatementIndentStack;
      vector<int> *inStatementIndentStackSizeStack;
      vector<int> *parenIndentStack;
      vector<bool> *bracketBlockStateStack;
      string indentString;
      const string *currentHeader;
      const string *previousLastLineHeader;
      const string *immediatelyPreviousAssignmentOp;
      const string *probationHeader;
      bool isInQuote;
      bool isInComment;
      bool isInCase;
      bool isInQuestion;
      bool isInStatement;
      bool isInHeader;
      bool isCStyle;
      bool isInOperator;
      bool isInTemplate;
      bool isInConst;
      bool isInDefine;
      bool isInDefineDefinition;
      bool classIndent;
      bool isInClassHeader;
      bool isInClassHeaderTab;
      bool switchIndent;
      bool caseIndent;
      bool namespaceIndent;
      bool bracketIndent;
      bool blockIndent;
      bool labelIndent;
      bool preprocessorIndent;
      bool isInConditional;
      bool isMinimalConditinalIndentSet;
      bool shouldForceTabIndentation;
      int minConditionalIndent;
      int parenDepth;
      int indentLength;
      int blockTabCount;
      unsigned int leadingWhiteSpaces;
      int maxInStatementIndent;
      int templateDepth;
      char quoteChar;
      char prevNonSpaceCh;
      char currentNonSpaceCh;
      char currentNonLegalCh;
      char prevNonLegalCh;
      int prevFinalLineSpaceTabCount;
      int prevFinalLineTabCount;
      bool emptyLineFill;
      bool backslashEndsPrevLine;
      int defineTabCount;
    };
}

#endif
/*
 * Copyright (c) 1998,1999,2000,2001,2002 Tal Davidson. All rights reserved.
 *
 * ASFormatter.cpp
 * by Tal Davidson (davidsont@bigfoot.com)
 * This file is a part of "Artistic Style" - an indentater and reformatter
 * of C, C++, C# and Java source files.
 *
 * The "Artistic Style" project, including all files needed to compile it,
 * is free software; you can redistribute it and/or use it and/or modify it
 * under the terms of the GNU General Public License as published 
 * by the Free Software Foundation; either version 2 of the License, 
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program.
 *
 *
 * Patches:
 * 26 November 1998 - Richard Bullington -
 *        A correction of line-breaking in headers following '}',
 
 *        was created using a variation of a  patch by Richard Bullington.
 * 08 May 2004
 *        applied   ASFormatter450670.patch.bz2, ASFormatter.cpp.patch.bz2,
 *                  patch1_ssvb_patch.tar.gz
 */

#include "compiler_defines.h"
#include "ASFormatter.h"


#include <string>
#include <cctype>
#include <vector>
#include <algorithm>
#include <iostream>


#define INIT_CONTAINER(container, value)     {if ( (container) != NULL ) delete (container); (container) = (value); }
#define DELETE_CONTAINER(container)          {if ( (container) != NULL ) delete (container) ; }
#define IS_A(a,b)                            ( ((a) & (b)) == (b))
#ifdef USES_NAMESPACE
using namespace std;

