#include <iostream>
#include "hw3_output.hpp"
#include <sstream>

using namespace std;

const std::string output::rules[] = {
        "Program -> Funcs",
        "Funcs -> epsilon",
        "Funcs -> FuncDecl Funcs",
        "FuncDecl -> OverRide RetType ID LPAREN Formals RPAREN LBRACE Statements RBRACE",
	    "OverRide -> epsilon",
	    "OverRide -> OVERRIDE",
        "RetType -> Type",
        "RetType ->  VOID",
        "Formals -> epsilon",
        "Formals -> FormalsList",
        "FormalsList -> FormalDecl",
        "FormalsList -> FormalDecl COMMA FormalsList",
        "FormalDecl -> Type ID",
        "Statements -> Statement",
        "Statements -> Statements Statement",
        "Statement -> LBRACE Statements RBRACE",
        "Statement -> Type ID SC",
        "Statement -> Type ID ASSIGN Exp SC",
        "Statement -> ID ASSIGN Exp SC",
        "Statement -> Call SC",
        "Statement -> RETURN SC",
        "Statement -> RETURN Exp SC",
        "Statement -> IF LPAREN Exp RPAREN Statement",
        "Statement -> IF LPAREN Exp RPAREN Statement ELSE Statement",
        "Statement -> WHILE LPAREN Exp RPAREN Statement",
        "Statement -> BREAK SC",
        "Statement -> CONTINUE SC",
        "Call -> ID LPAREN ExpList RPAREN",
        "Call -> ID LPAREN RPAREN",
        "ExpList -> Exp",
        "ExpList -> Exp COMMA ExpList",
        "Type -> INT",
        "Type -> BYTE",
        "Type -> BOOL",
        "Exp -> LPAREN Exp RPAREN",
        "Exp -> Exp BINOP Exp",
        "Exp -> ID",
        "Exp -> Call",
        "Exp -> NUM",
        "Exp -> NUM B",
        "Exp -> STRING",
        "Exp -> TRUE",
        "Exp -> FALSE",
        "Exp -> NOT Exp",
        "Exp -> Exp AND Exp",
        "Exp -> Exp OR Exp",
        "Exp -> Exp RELOP Exp",
        "Exp -> LPAREN Type RPAREN Exp"
};

void output::printProductionRule(const int ruleno) {
    Log() << ruleno << ": " << output::rules[ruleno-1] << "\n";
    Log() << "===================== Reduced =====================" << std::endl;
}



Frame_class frame_manager;

Frame_class& Frame_class::getInstance() {
    return frame_manager;
}


void output::endScope(){
    cout << "---end scope---" << endl;
}

void output::printID(const string& id, int offset, const string& type) {
    cout << id << " " << type <<  " " << offset <<  endl;
}

// Builds a string from vector of types, and returns it
string typeListToString(const std::vector<string>& argTypes) {
    stringstream res;
    res << "(";
    for(int i = 0; i < argTypes.size(); ++i) {
        res << argTypes[i];
        if (i + 1 < argTypes.size())
            res << ",";
    }
    res << ")";
    return res.str();
}

// Builds a string from vector of values, and returns it
string valueListsToString(const std::vector<string>& values) {
    stringstream res;
    res << "{";
    for(int i = 0; i < values.size(); ++i) {
        res << values[i];
        if (i + 1 < values.size())
            res << ",";
    }
    res << "}";
    return res.str();
}

string output::makeFunctionType(const string& retType, std::vector<string>& argTypes) {
    stringstream res;
    res << typeListToString(argTypes) << "->" << retType;
    return res.str();
}

void output::errorLex(int lineno){
    cout << "line " << lineno << ":" << " lexical error" << endl;
}

void output::errorSyn(int lineno){
    cout << "line " << lineno << ":" << " syntax error" << endl;
}

void output::errorUndef(int lineno, const string& id){
    cout << "line " << lineno << ":" << " variable " << id << " is not defined" << endl;
}

void output::errorDef(int lineno, const string& id){
    cout << "line " << lineno << ":" << " identifier " << id << " is already defined" << endl;
}

void output::errorUndefFunc(int lineno, const string& id) {
    cout << "line " << lineno << ":" << " function " << id << " is not defined" << endl;
}

void output::errorMismatch(int lineno){
    cout << "line " << lineno << ":" << " type mismatch" << endl;
}

void output::errorPrototypeMismatch(int lineno, const string& id) {
    cout << "line " << lineno << ": prototype mismatch, function " << id << endl;
}

void output::errorUnexpectedBreak(int lineno) {
    cout << "line " << lineno << ":" << " unexpected break statement" << endl;
}

void output::errorUnexpectedContinue(int lineno) {
    cout << "line " << lineno << ":" << " unexpected continue statement" << endl;	
}

void output::errorMainMissing() {
    cout << "Program has no 'void main()' function" << endl;
}

void output::errorByteTooLarge(int lineno, const string& value) {
    cout << "line " << lineno << ": byte value " << value << " out of range" << endl;
}


void output::errorFuncNoOverride(int lineno, const string& id) {
    cout << "line " << lineno << ": function " << id << " was declared before as non-override function" << endl;
}

void output::errorOverrideWithoutDeclaration(int lineno, const string& id) {
    cout << "line " << lineno << ": function " << id << " attempt to override a function without declaring the current function as override" << endl;
}

void output::errorAmbiguousCall(int lineno, const string& id) {
    cout << "line " << lineno << ": ambiguous call to overloaded function " << id << endl;
}

void output::errorMainOverride(int lineno){
    cout << "line " << lineno << ": main is not allowed to be overridden" << endl;
}


bool valid_cast(Type to, Type from){
    if (from == to){
        return true;
    }
    if (to == Type::INT || to == Type::BYTE){
        if (from == Type::INT || from == Type::BYTE){
            return true;
        }
    }
    return false;
}

bool valid_implicit_cast(Type to, Type from) {
    if (to == from) {
        return true;
    }
    if (to == Type::INT && from == Type::BYTE) {
        return true;
    }
    return false;
}


/// ############################################################################## ///
/// ############################    Symbol    ############################///
/// ############################################################################## ///
// Takes a vector of symbols and returns a vector of corresponding types
std::vector<Type> symbolToTypeVec(std::vector<Symbol> func_params){
    std::vector<Type> typeVec = {};
    for (auto iter = func_params.begin(); iter != func_params.end(); iter++){
        typeVec.push_back((*iter).type);
    }
    return typeVec;
}


/// ############################################################################## ///
/// #####################    symTableEntryID + symTableEntryFunc   ###############///
/// ############################################################################## ///

void symTableEntryID::print() const{
    output::printID(symbol.name, offset, TypeToSTR(symbol.type));
}

void symTableEntryFunc::print() const{
    std::vector<string> a = typeToStrVector(paramsToTypeVec());
    cout << symbol.name << " " << output::makeFunctionType(TypeToSTR(symbol.type),a) << " " << offset << std::endl;
}

/// ############################################################################## ///
/// ############################    Node_ExpList    ############################///
/// ############################################################################## ///

Node_ExpList::Node_ExpList(Node_Exp* exp) : Generic_Node({exp}){
    exp_list.clear();
    exp_list.push_back(exp->type);
}

Node_ExpList::Node_ExpList(Node_Exp* node_exp, Node_Token* node_token,
                           Node_ExpList* node_expList): Generic_Node({node_exp, node_token, node_expList}) {
    Log() << "Node_ExpList():: 1" << std::endl;
    exp_list = node_expList->exp_list;
    exp_list.insert(exp_list.begin(), node_exp->type);
    Log() << "Node_ExpList():: 2" << std::endl;
}


/// ############################################################################## ///
/// ############################    Node_FormalDecl    ############################///
/// ############################################################################## ///

Node_FormalDecl::Node_FormalDecl(Node_Exp* node_type, Node_Token* node_token_id)
                : Generic_Node({node_type, node_token_id}), id_symbol(node_type->type, node_token_id->value){
    
    if (frame_manager.find(id_symbol.name) != nullptr){
        throw DefExc(yylineno, id_symbol.name);
    }
    
}

/// ############################################################################## ///
/// ############################    Node_Program    ############################///
/// ############################################################################## ///

Node_Program::Node_Program(Node_FuncsList* node_funcsList):
    Generic_Node({node_funcsList}){
}

/// ############################################################################## ///
/// ############################    Node_FormalsList    ############################///
/// ############################################################################## ///

Node_FormalsList::Node_FormalsList(Node_FormalDecl* node_formalDecl): Generic_Node({node_formalDecl}) {
    parameter_list.emplace_back(node_formalDecl->id_symbol);
}

Node_FormalsList::Node_FormalsList(Node_FormalDecl* node_formalDecl,
                                   Node_Token* node_comma, Node_FormalsList* node_formalsList)
                                   : Generic_Node({node_formalDecl, node_comma, node_formalsList}) {
    parameter_list = node_formalsList->parameter_list;
    parameter_list.insert(parameter_list.begin(), node_formalDecl->id_symbol);
}

/// ############################################################################## ///
/// ############################    Node_Formals    ############################///
/// ############################################################################## ///
Node_Formals::Node_Formals(): Generic_Node({}) {

}

Node_Formals::Node_Formals(Node_FormalsList* node_formalsList): Generic_Node({node_formalsList}) {
    parameter_list = node_formalsList->parameter_list;
}


/// ############################################################################## ///
/// ############################    Node_Override    ###########################///
/// ############################################################################## ///
Node_Override::Node_Override(): Generic_Node({}) {

}

Node_Override::Node_Override(Node_Token* node_override): Generic_Node({node_override}) {

}


/// ############################################################################## ///
/// ############################    Node_Exp_ID    ############################///
/// ############################################################################## ///

Node_Exp_ID::Node_Exp_ID(Node_Token* node_token) : Node_Exp({node_token}, Type::INVALID), id(Symbol::invalidSymbol()) {
    Log() << "Node_Exp_ID(" << node_token->value << ")"  << std::endl;
    auto entry = frame_manager.find(node_token->value);
    if (entry == nullptr) {
        Log() << "Node_Exp_ID()::UndefExc" << std::endl;
        throw UndefExc(yylineno, node_token->value);
    }
    
    if (entry->entry_type != DeclType::VAR){
        throw UndefExc(yylineno, node_token->value);
    }
    Log() << "Node_Exp_ID()::PASSED" << std::endl;
    set_type(entry->symbol.type);
    id = entry->symbol;
}

/// ############################################################################## ///
/// ############################    Node_Exp_Bool    ############################///
/// ############################################################################## ///

Node_Exp_Bool::Node_Exp_Bool(Node_Token *node_token) : Node_Exp({node_token},Type::BOOL){
}

Node_Exp_Bool::Node_Exp_Bool(Node_Token *node_not, Node_Exp *node_exp) : Node_Exp({node_not, node_exp},Type::BOOL){
    if (!node_exp->typeCheck({Type::BOOL})){
        throw MismatchExc(yylineno);
    }
}

Node_Exp_Bool::Node_Exp_Bool(Node_Exp *node_exp1, Node_Token *node_AndOR, Node_Exp *node_exp2)
                                : Node_Exp({node_exp1, node_AndOR, node_exp2}, Type::BOOL){
    if (!node_exp1->typeCheck({Type::BOOL}) || !node_exp2->typeCheck({Type::BOOL})){
        throw MismatchExc(yylineno);
    }
}

Node_Exp_Bool::Node_Exp_Bool(Node_Exp *node_exp) : Node_Exp({node_exp}, node_exp->type){
    Log() << "ExpBool Reduced" << std::endl;
    if (!node_exp->typeCheck({Type::BOOL})){
        throw MismatchExc(yylineno);
    }
}

/// ############################################################################## ///
/// ############################    Node_Exp_Relop    ############################///
/// ############################################################################## ///

Node_Exp_Relop::Node_Exp_Relop(NodeVector children) : Node_Exp(children, Type::BOOL) {
    auto exp1 = (Node_Exp*)(children[0]);
    auto exp2 = (Node_Exp*)(children[2]);
    Log() << "Node_Exp_Relop"<<std::endl;
    if (!exp1->typeCheck({Type::INT, Type::BYTE}) || !exp2->typeCheck({Type::INT, Type::BYTE})) {
        throw MismatchExc(yylineno);
    }
}
/// ############################################################################## ///
/// ############################    Node_Exp_Cast    ############################///
/// ############################################################################## ///


Node_Exp_Cast::Node_Exp_Cast(NodeVector children) : Node_Exp(children, Type::INVALID) {
    auto type_node = (Node_Exp_Type*)(children[1]);
    auto exp = (Node_Exp*)(children[3]);
    
    Log() << "Node_Exp_Cast::to= "<< TypeToSTR(type_node->type) << ", from= "<< TypeToSTR(exp->type) << std::endl;
    if (!valid_cast(type_node->type, exp->type)) {
        Log() << "Node_Exp_Cast::MismatchExc"<<std::endl;
        throw MismatchExc(yylineno);
    }
    set_type(type_node->type);
}


/// ############################################################################## ///
/// ############################    Node_Call    ############################///
/// ############################################################################## ///

// Call with parameters
Node_Call::Node_Call(Node_Token* node_id, Node_Token* node_lparen,
                     Node_ExpList* node_expList, Node_Token* node_rparen)
                     : Generic_Node({node_id, node_lparen, node_expList, node_rparen}), func_id(Symbol::invalidSymbol()){
    // check if func declared
    auto id_entry = frame_manager.find(node_id->value);
    if (id_entry == nullptr || id_entry->entry_type != DeclType::FUNC){
        throw UndDefFuncExc(yylineno, node_id->value);
    }
    
    func_id = Symbol(id_entry->symbol.type, id_entry->symbol.name);
    func_parameters = node_expList->exp_list; // Expected parameters

    // check if func prototype matches func call
    auto func_entry = dynamic_cast<symTableEntryFunc*>(id_entry);
    Log() << "Node_Call:: size1=" << func_entry->parameter_list.size() << "size2=" << func_parameters.size() <<std::endl;
    if (func_entry->parameter_list.size() != func_parameters.size()){ // If the size doesn't match, we can already tell there's a mismatch
        throw PrototypeMismatchExc(yylineno, func_id.name);
    }

    for (int index = 0; index < func_entry->parameter_list.size(); index++){ // Checking match for each paramter
        if (!valid_cast(func_entry->parameter_list[index].type, func_parameters[index])){
            throw PrototypeMismatchExc(yylineno, func_id.name);
        }
    }
}

// Call without parameters
Node_Call::Node_Call(Node_Token* node_id, Node_Token* node_lparen, Node_Token* node_rparen)
        : Generic_Node({node_id, node_lparen, node_rparen}), func_id(Symbol::invalidSymbol()){
    // check if func declared
    auto id_entry = frame_manager.find(node_id->value);
    if (id_entry == nullptr || id_entry->entry_type != DeclType::FUNC){
        throw UndDefFuncExc(yylineno, node_id->value);
    }
    func_id = Symbol(id_entry->symbol.type, id_entry->symbol.name);
    func_parameters = {}; // Expected parameters (none)
    // check if func prototype matches func call
    auto func_entry = dynamic_cast<symTableEntryFunc*>(id_entry);
    if (func_entry->parameter_list.size() != func_parameters.size()){ // If the size doesn't match, there's a mismatch
        throw PrototypeMismatchExc(yylineno, func_id.name);
    }   
}


/// ############################################################################## ///
/// ############################    Node_Statement    ############################///
/// ############################################################################## ///

Node_Statement::Node_Statement(NodeVector children): Generic_Node(children) {
}

/// ############################################################################## ///
/// ############################    Node_Statement_ID_Decl    ############################///
/// ############################################################################## ///

Node_Statement_ID_Decl::Node_Statement_ID_Decl(Node_Exp *node_type,
                                               Node_Token* node_token,
                                               Node_Token* node_sc)
                                               : Node_Statement({node_type, node_token, node_sc}) {
    frame_manager.newEntry(DeclType::VAR, node_token->value, node_type->type);
}

Node_Statement_ID_Decl::Node_Statement_ID_Decl(Node_Exp* node_type,
                                               Node_Token* node_token,
                                               Node_Token* node_assign,
                                               Node_Exp* node_exp,
                                               Node_Token* node_sc)
                                               : Node_Statement({node_type, node_token, node_assign, node_exp, node_sc}) {
    
    Node_Exp_Type* node_type_p = dynamic_cast<Node_Exp_Type*>(node_type);
    frame_manager.newEntry(DeclType::VAR, node_token->value, node_type_p->type);
    
    if (!valid_implicit_cast(node_type_p->type, node_exp->type)){
        //frame_manager.removeEntryFromCurrentScope(node_token->value);
        throw MismatchExc(yylineno);
    }
    Log() << "Node_Statement_ID_Decl:: " << TypeToSTR(node_type->type) << " " << node_token->value << std::endl;
}


/// ############################################################################## ///
/// ############################    Node_Statement_ID_Assign    ############################///
/// ############################################################################## ///

Node_Statement_ID_Assign::Node_Statement_ID_Assign(Node_Token* node_id,
                                                   Node_Token* node_assign,
                                                   Node_Exp* node_exp,
                                                   Node_Token* node_sc)
                                                   : Node_Statement({node_id, node_assign, node_exp, node_sc}) {
    
    auto id_entry = frame_manager.find(node_id->value);
    if (id_entry == nullptr || id_entry->entry_type != DeclType::VAR){
        throw UndefExc(yylineno, node_id->value);
    }
    
    if (!valid_implicit_cast(id_entry->symbol.type, node_exp->type)){
        throw MismatchExc(yylineno);
    }
}


/// ############################################################################## ///
/// ############################    Node_Statement_Call    ############################///
/// ############################################################################## ///

Node_Statement_Call::Node_Statement_Call(Node_Call* node_call, Node_Token* node_sc)
                                        : Node_Statement({node_call, node_sc}){
}



/// ############################################################################## ///
/// ############################    Node_Statement_Ret    ############################///
/// ############################################################################## ///


Node_Statement_Ret::Node_Statement_Ret(Node_Token* node_ret, Node_Token* node_sc)
                                        : Node_Statement({node_ret, node_sc}){
    
    if (frame_manager.scopeRetType() != Type::VOID){
        throw MismatchExc(yylineno);
    }
}


Node_Statement_Ret::Node_Statement_Ret(Node_Token* node_ret, Node_Exp* node_exp,
                                       Node_Token* node_sc)
                                       : Node_Statement({node_ret, node_exp, node_sc}) {
    
    if (frame_manager.scopeRetType() == Type::VOID || valid_implicit_cast(frame_manager.scopeRetType(), node_exp->type) == false){
        throw MismatchExc(yylineno);
    }
}

/// ############################################################################## ///
/// ############################    Node_Statement_IF    ############################///
/// ############################################################################## ///

Node_Statement_IF::Node_Statement_IF(Node_Token *node_if, Node_Token *node_lparen, Node_Exp *node_exp,
                                     Node_Token *node_rparen, Node_Statement *node_statement)
                                     : Node_Statement({node_if, node_lparen, node_exp, node_rparen, node_statement}){
    
    if (node_exp->typeCheck({Type::BOOL}) == false){
        throw MismatchExc(yylineno);
    }
}

Node_Statement_IF::Node_Statement_IF(Node_Token *node_if, Node_Token *node_lparen, Node_Exp *node_exp,
                                     Node_Token *node_rparen, Node_Statement *node_statement1, Node_Token *node_else,
                                     Node_Statement *node_statement2)
                                     :  Node_Statement({node_if, node_lparen, node_exp, node_rparen, node_statement1, node_else, node_statement2}){
    
    if (node_exp->typeCheck({Type::BOOL}) == false){
        throw MismatchExc(yylineno);
    }
}

/// ############################################################################## ///
/// ############################    Node_Statement_While    ############################///
/// ############################################################################## ///

Node_Statement_While::Node_Statement_While(Node_Token* node_while,
                                           Node_Token* node_lparen, Node_Exp* node_exp,
                                           Node_Token* node_rparen,
                                           Node_Statement* node_statement)
                                           : Node_Statement({node_while, node_lparen, node_exp, node_rparen, node_statement}) {
    
    if (node_exp->typeCheck({Type::BOOL}) == false){
        throw MismatchExc(yylineno);
    }
}


/// ############################################################################## ///
/// ############################    Node_Statement_LoopMod    ############################///
/// ############################################################################## ///

Node_Statement_LoopMod::Node_Statement_LoopMod(Node_Token* node_loop_mod, Node_Token* node_sc)
                                                : Node_Statement({node_loop_mod, node_sc}){
    
    if (!frame_manager.inLoop()){
        if (node_loop_mod->value == "break"){
            throw UnexpectedBreakExc(yylineno);
        }
        throw UnexpectedContinueExc(yylineno);
    }
}


/// ############################################################################## ///
/// ############################    Node_FuncDecl    ############################///
/// ############################################################################## ///

Node_FuncDecl::Node_FuncDecl(Node_Override* node_override, Node_RetType* node_retType, Node_Token* node_id,
                             Node_Token* node_lparen, Node_Formals* node_formals,
                             Node_Token* node_rparen, Node_Token* node_lbrace,
                             Node_Statement* node_statement, Node_Token* node_rbrace)
                             : Generic_Node({node_override, node_retType, node_id, node_lparen, node_formals, node_rparen, node_lbrace, node_statement, node_rbrace}){
     
}

void Node_FuncDecl::newFuncFrame(Node_Override* node_override, Node_RetType *node_retType, Node_Token *node_id, Node_Token *node_lparen,
                             Node_Formals *node_formals, Node_Token * node_rparen){
    Log() << "newFuncFrame:: " << node_id->value << std::endl;
    frame_manager.newEntry(DeclType::FUNC, node_id->value, node_retType->type, node_formals->parameter_list);
    
    Log() << "newFuncFrame:: " << node_id->value << std::endl;
    frame_manager.newFrame(FrameType::FUNC, node_id->value);
}




void StackEntry::addFuncParams(std::vector<Symbol> func_params){
    int offset = next_offset-1;
    for (auto iter = func_params.begin(); iter != func_params.end(); iter++){
        if (frame_manager.find(iter->name) != nullptr){
            throw DefExc(yylineno, iter->name);
        }
        auto entry = new symTableEntryID((*iter), DeclType::VAR, offset);
        entries.insert({entry->symbol.name, entry});
        entries_vector.push_back(entry);
        offset--;
    }
    
}