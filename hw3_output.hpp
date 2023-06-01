
#ifndef _236360_3_
#define _236360_3_

#include <string>

#include <vector>
#include <memory>
#include <map>
#include <ostream>
#include "iostream"

//-----------------MISC-----------------//
// For debugging, disabled now
static std::ostream dev_null{nullptr};
#define Debug_LVL 0
static std::ostream& Log(int level=90){
    if (level <= Debug_LVL){
        return std::cout << "Log()::";
    }
    return dev_null;
}

extern int yylineno;

namespace output{
    extern const std::string rules[];
    void endScope();
    void printID(const std::string& id, int offset, const std::string& type);

    /* Do not save the string returned from this function in a data structure
        as it is not dynamically allocated and will be destroyed(!) at the end of the calling scope. */
    std::string makeFunctionType(const std::string& retType, std::vector<std::string>& argTypes);

    void errorLex(int lineno);
    void errorSyn(int lineno);
    void errorUndef(int lineno, const std::string& id);
    void errorDef(int lineno, const std::string& id);
    void errorUndefFunc(int lineno, const std::string& id);
    void errorMismatch(int lineno);
    void errorPrototypeMismatch(int lineno, const std::string& id);
    void errorUnexpectedBreak(int lineno);
    void errorUnexpectedContinue(int lineno);
    void errorMainMissing();
    void errorByteTooLarge(int lineno, const std::string& value);
    void errorFuncNoOverride(int lineno, const std::string& id);
    void errorOverrideWithoutDeclaration(int lineno, const std::string& id);
    void errorAmbiguousCall(int lineno, const std::string& id);
    void errorMainOverride(int yylineno);
    void printProductionRule(const int ruleno); // Ours
}


enum class Type { INVALID = 0, INT, BYTE, BOOL, STRING, VOID, TOKEN };
enum class DeclType { INVALID = 0, VAR, FUNC };
enum class FrameType { FUNC, LOOP, BLOCK, IF_WHILE, GLOBAL };
static std::vector<std::string> TypeToSTRVec = {"INVALID", "INT", "BYTE", "BOOL", "STRING", "VOID", "TOKEN"};

static std::string TypeToSTR(Type type){
    return TypeToSTRVec.at(static_cast<unsigned long>(type));
}

// Takes vector of types, returns a vector of corresponding strings
static std::vector<std::string> typeToStrVector(std::vector<Type> vec){
    std::vector<std::string> str_vec;
    for (auto iter = vec.begin(); iter != vec.end(); iter++){
        str_vec.push_back(TypeToSTR(*iter));
    }
    return str_vec;
}

//-----------------EXCEPTIONS-----------------//
class AppaException : public std::exception {
public:
    long lineno;
    
    AppaException(long line_number) : std::exception() {
        lineno = line_number;
    }
};

class Lexception : public AppaException {
public:
    
    Lexception(long lineno) : AppaException(lineno) {
        output::errorLex(lineno);
    };
};

class SynExc : public AppaException {
public:
    SynExc(long lineno) : AppaException(lineno) {
        output::errorSyn(lineno);
    };
};

class UndefExc : public AppaException {
public:
    UndefExc(long lineno, std::string& id) : AppaException(lineno) {
        output::errorUndef(lineno, id);
    };
};

class DefExc : public AppaException {
public:
    DefExc(long lineno, std::string& id) : AppaException(lineno) {
        output::errorDef(lineno, id);
    };
};

class UndDefFuncExc : public AppaException {
public:
    UndDefFuncExc(long lineno, std::string& id) : AppaException(lineno) {
        output::errorUndefFunc(lineno, id);
    };
};

class MismatchExc : public AppaException {
public:
    MismatchExc(long lineno) : AppaException(lineno) {
        output::errorMismatch(lineno);
    };
};

class PrototypeMismatchExc : public AppaException {
public:
    PrototypeMismatchExc(long lineno, std::string& id) : AppaException(lineno) {
        output::errorPrototypeMismatch(lineno, id);
    };
};

class UnexpectedBreakExc : public AppaException {
public:
    UnexpectedBreakExc(long lineno) : AppaException(lineno) {
        output::errorUnexpectedBreak(lineno);
    };
};

class UnexpectedContinueExc : public AppaException {
public:
    UnexpectedContinueExc(long lineno) : AppaException(lineno) {
        output::errorUnexpectedContinue(lineno);
    };
};

class MainMissingExc : public AppaException {
public:
    MainMissingExc() : AppaException(0) {
        output::errorMainMissing();
    };
};

class ByteTooLargeExc : public AppaException {
public:
    ByteTooLargeExc(long lineno, std::string& value) : AppaException(lineno) {
        output::errorByteTooLarge(lineno, value);
    }
};

class FuncNoOverrideExc : public AppaException {
public:
    FuncNoOverrideExc(int lineno, const std::string& id) : AppaException(lineno) {
        output::errorFuncNoOverride(lineno, id);
    }
};

class OverrideWithoutDeclarationExc : public AppaException {
public:
    OverrideWithoutDeclarationExc(int lineno, const std::string& id) : AppaException(lineno) {
        output::errorOverrideWithoutDeclaration(lineno, id);
    }
};

class AmbiguousCallExc : public AppaException {
public:
    AmbiguousCallExc(int lineno, const std::string& id) : AppaException(lineno) {
        output::errorAmbiguousCall(lineno, id);
    }
};

class MainOverrideExc : public AppaException {
public:
    MainOverrideExc(int yylineno) : AppaException(yylineno) {
        output::errorMainOverride(yylineno);
    }
};

//-----------------CLASSES-----------------//
class Generic_Node;
class symTableEntry;
class StackEntry;
class Frame_class;
class Node_FormalDecl;
class Node_Exp_Type;
class Node_Statement;
class Node_Exp;
class Node_FuncsList;

typedef Generic_Node* Node;
typedef std::vector<Node> NodeVector;
typedef symTableEntry* SymEntry;
typedef std::multimap<std::string, SymEntry> dict;
typedef std::vector<StackEntry> frame;

//-----------------SCOPE CLASSES-----------------//
class Symbol {
public:
    Type type;
    std::string name;
    
    Symbol(Type sym_type, std::string sym_name) : type(sym_type), name(sym_name) {
    }
    
    Symbol(Symbol&) = default;
    
    Symbol(Symbol const&) = default;
    
    Symbol& operator=(Symbol const& sym) = default;
    
    static Symbol invalidSymbol() {
        return Symbol(Type::INVALID, "");
    }
};


// Returns a vector of types, corresponding to the vector of parameters
static std::vector<Type> paramsToTypeVec(std::vector<Symbol> parameter_list) {
    std::vector<Type> type_vec;
    for (auto iter = parameter_list.begin(); iter != parameter_list.end(); iter++){
        type_vec.push_back(iter->type);
    }
    return type_vec;
}


class symTableEntry {
public:
    Symbol symbol;
    DeclType entry_type;
    long offset;
    
    symTableEntry(Symbol sym, DeclType entry_type, long entry_offset, bool entry_valid = true) : symbol(sym) {
        this->entry_type = entry_type;
        offset = entry_offset;
    }
    
    ~symTableEntry() = default;
    
    symTableEntry(symTableEntry& entry) = default;
    
    virtual void print() const=0;
    
    friend std::ostream& operator<<(std::ostream& os, const symTableEntry& entry) {
        entry.print();
        return os;
    };
};

// Table entry for variables
class symTableEntryID : public symTableEntry {
public:
    symTableEntryID(Symbol sym, DeclType entry_type, long entry_offset) : symTableEntry(sym, entry_type, entry_offset, true) {
    }
    ~symTableEntryID() = default;
    symTableEntryID(symTableEntryID& entry) = default;
    
    void print() const override;
};

// Table entry for functions
class symTableEntryFunc : public symTableEntry {
public:
    std::vector<Symbol> parameter_list;
    bool is_override;

    symTableEntryFunc(Symbol sym, DeclType entry_type, long entry_offset, std::vector<Symbol> func_params, bool is_override)
            : symTableEntry(sym, entry_type, entry_offset, true) {
        parameter_list = func_params;
        this->is_override = is_override;
    }
    ~symTableEntryFunc() = default;
    symTableEntryFunc(symTableEntryFunc& entry) = default;

    void print() const override;
};

// A scope frame has stack entries in it
class StackEntry {
public:
    long next_offset;
    FrameType frame_type;
    SymEntry scope_func_entry;
    bool inside_loop;
    dict entries;
    std::vector<SymEntry> entries_vector;
    
    StackEntry(FrameType frame_type, bool in_loop, SymEntry& scope_func, long offset=0) {
        this->frame_type = frame_type;
        inside_loop = in_loop;
        scope_func_entry = scope_func;
        next_offset = offset;

        if (frame_type == FrameType::FUNC){
            Log() << "StackEntry:: offset=" << next_offset << std::endl;
        }
    };
    
    ~StackEntry() = default;
    StackEntry(StackEntry& ) = default;
    StackEntry(StackEntry const&) = default;
    
    void newIdEntry(Symbol sym) { // ID
        auto entry = new symTableEntryID(sym, DeclType::VAR, next_offset);
        next_offset++;
        
        entries.insert({entry->symbol.name, entry});
        entries_vector.push_back(entry);
    }
    symTableEntryFunc* newFuncEntry(Symbol sym, std::vector<Symbol> func_params, bool is_override) { // Function
        symTableEntryFunc* entry = new symTableEntryFunc(sym, DeclType::FUNC, 0, func_params, is_override);
        
        entries.insert({entry->symbol.name, entry});
        entries_vector.push_back(entry);

        return entry;
    }
    void addFuncParams(std::vector<Symbol> func_params);
    
    // Finds an identifier in a SCOPE
    SymEntry find(std::string name) {
        Log() << "StackEntry::find(" << name <<")";
        auto search = entries.find(name); // builtin find() for STL dictionary
        
        if (search == entries.end()) {
            Log() << " -> NO" << std::endl;
            return nullptr;
        }
        Log() << " -> YES" << std::endl;
        return search->second;
    }
    
    friend std::ostream& operator<<(std::ostream& os, const StackEntry& frame) {
        output::endScope();
        for (auto iter = frame.entries_vector.begin(); iter != frame.entries_vector.end(); iter++) {
            os << *(*iter);
        }
        return os;
    }
};

class Frame_class {
public:
    frame frames;
    
    Frame_class() {
        Symbol sym(Type::INVALID, "Global");
        SymEntry a = new symTableEntryID(sym, DeclType::INVALID, 0);
        frames.emplace_back(FrameType::GLOBAL, false, a);
    
        frames.back().newFuncEntry(Symbol(Type::VOID, "print"), {Symbol(Type::STRING, "str")}, false);
        frames.back().newFuncEntry(Symbol(Type::VOID, "printi"), {Symbol(Type::INT, "str")}, false);
        Log() << "Frame_class::Constructor Done" << std::endl;
    };
    ~Frame_class() = default;
    Frame_class(Frame_class&) = delete;
    
    static Frame_class& getInstance();
    
    // For IDs
    void newEntryID(std::string name, Type id_type) {
        Log(10) << "newEntry(ID, " << name << ")" << std::endl;
        SymEntry entry = findID(name);
        if (entry != nullptr) {
            throw DefExc(yylineno, name);
        }
        frames.back().newIdEntry(Symbol(id_type, name));
    }

    /*// For functions - adds an entry in the global scope
    void newEntryFunction(std::string name, Type ret_type, std::vector<Symbol> func_params, bool is_override) {
        /* // Example: void foo(int a); int foo();
        if ( entry && !entry->is_override && !is_override ) {
            throw DefExc(yylineno, name);
        }
        // Example: override int foo(int a); override int foo(int b);
        if ( entry && entry->is_override && is_override && (paramsToTypeVec(entry->parameter_list) == paramsToTypeVec(func_params)) ){
            throw DefExc(yylineno, name);
        }
        // Example: foo(int a); override foo();
        if (entry && !entry->is_override && is_override){
            throw FuncNoOverrideExc(yylineno, name);
        }
        // Example: override foo(int a); foo();
        if (entry && entry->is_override && !is_override){
            throw OverrideWithoutDeclarationExc(yylineno, name);
        }

        frames.back().newFuncEntry(Symbol(ret_type, name), func_params, is_override);
    } */


    // For ID
    void newFrame(FrameType frame_type) {
        Log() << "newFrame()" << std::endl;
        auto &curr_frame = frames.back();
        bool in_loop = (frame_type == FrameType::LOOP) || curr_frame.inside_loop;
        frames.emplace_back(frame_type, in_loop, curr_frame.scope_func_entry, curr_frame.next_offset);
    }

    /*// For functions - adds the function parameters to its new entry
    void newFrame(FrameType frame_type, std::string scope_func) {
        SymEntry func_entry = findFunction(scope_func);
        frames.emplace_back(frame_type, false, func_entry);
        frames.back().addFuncParams(dynamic_cast<symTableEntryFunc*>(func_entry)->parameter_list);
    } */
    
    // Finds an ID in the entire STACK of scopes
    SymEntry findID(std::string name) {
        for (auto iter = frames.rbegin(); iter != frames.rend(); iter++) {
            SymEntry entry = iter->find(name); // Occurance of StackEntry::find()
            if (entry != nullptr) return entry;
        }
        return nullptr;
    }

    // Finds a function in the entire STACK of scopes
    std::vector<symTableEntryFunc*> findFunction(std::string name) {
        StackEntry global_scope = frames[0];
        std::vector<symTableEntryFunc*> matching_funcs = {};

        for (auto& pair : global_scope.entries){ // Finding all of the functions with the same name
            symTableEntryFunc* entry = dynamic_cast<symTableEntryFunc*>(pair.second);
            if (name == entry->symbol.name) matching_funcs.push_back(entry);
        }

        return matching_funcs;

        /*for (auto& pair : global_scope.entries){
            symTableEntryFunc* entry = dynamic_cast<symTableEntryFunc*>(pair.second);
            std::vector<Type> pair_type_vec = paramsToTypeVec(entry->parameter_list);
            if (name == entry->symbol.name && type_vec == pair_type_vec) return entry;
        }

        for (auto& pair : global_scope.entries){
            symTableEntryFunc* entry = dynamic_cast<symTableEntryFunc*>(pair.second);
            std::vector<Type> pair_type_vec = paramsToTypeVec(entry->parameter_list);
            if ( name == entry->symbol.name && (pair_type_vec.size() == type_vec.size()) && valid_implicit_cast_vec(pair_type_vec, type_vec) ) return entry;
        } */
    }
    
    void closeFrame(){
        Log() << "--- Closing Frame ---" << std::endl;
        std::cout << frames.back();
        frames.pop_back();
    }
    
    /*void removeEntryFromCurrentScope(std::string name) {
        auto& scope = frames.back();
        scope.removeEntry(name);
    }*/
    bool inLoop() {
        return frames.back().inside_loop;
    }
    Type scopeRetType() {
        return frames.back().scope_func_entry->symbol.type;
    }
    
    bool mainDeclared(){
        std::vector <symTableEntryFunc*> matching_function = findFunction("main");
        if (matching_function.empty()){
            return false;
        }

        symTableEntryFunc* main_entry = matching_function[0];
        if (main_entry->symbol.type != Type::VOID || main_entry->parameter_list.size() != 0){
            return false;
        }
        return true;
    }
};


//-----------------AST CLASSES-----------------//
class Generic_Node {
    long node_tree_index;
public:
    Node parent;
    NodeVector children;

/////////// Methods ///////////
    // We pass the c'tor a vecotr of the node's children, and set them up
    Generic_Node(NodeVector children) {
        setChildren(children);
    }
    
    ~Generic_Node() = default;
    Generic_Node(Generic_Node&) = delete;
    
    void setChildren(NodeVector vector) {
        for (int index = 0; index < vector.size(); index++) {
            children.push_back(vector[index]);
            children[index]->setParent(this);
        }
    }
    void setParent(Node parent) {
        this->parent = parent;
    }
    
    // So that sub classes are polymorphic, and dynamic casting would work in the 'cpp' file, we need a virtual function
    virtual void stam() {};
};

class Node_Program : public Generic_Node {
public:
/////////// Methods ///////////
    Node_Program(Node_FuncsList* node_funcsList);
    
    ~Node_Program() = default;
    Node_Program(Node_Program&) = delete;
};

// Represents *all* tokens/terminals
class Node_Token : public Generic_Node {
public:
    std::string value;

/////////// Methods ///////////
    Node_Token(std::string token_value) : Generic_Node({}) {
        value = token_value;
        Log() << "Node_Token:: " << token_value << std::endl;
    }
    
    ~Node_Token() = default;
    Node_Token(Node_Token&) = delete;
};

class Node_RetType : public Generic_Node {
public:
    Type type;

/////////// Methods ///////////
    Node_RetType(NodeVector children, Type ret_type) : Generic_Node(children) {
        type = ret_type;
    }
    
    ~Node_RetType() = default;
    
    Node_RetType(Node_RetType&) = delete;
    
    void set_type(Type new_type) {
        type = new_type;
    };
};

class Node_FormalDecl : public Generic_Node {
public:
    Symbol id_symbol;

/////////// Methods ///////////
    Node_FormalDecl(Node_Exp* node_type, Node_Token* node_token_id);
    
    ~Node_FormalDecl() = default;
    
    Node_FormalDecl(Node_FormalDecl&) = delete;
};

class Node_FormalsList : public Generic_Node {
public:
    std::vector<Symbol> parameter_list;

/////////// Methods ///////////
    Node_FormalsList(Node_FormalDecl* node_formalDecl);
    
    Node_FormalsList(Node_FormalDecl* node_formalDecl, Node_Token* node_comma,
                     Node_FormalsList* node_formalsList);
    
    ~Node_FormalsList() = default;
    
    Node_FormalsList(Node_FormalsList&) = delete;
};

class Node_Formals : public Generic_Node {
public:
    std::vector<Symbol> parameter_list;
    
/////////// Methods ///////////
    Node_Formals(Node_FormalsList* node_formalsList);
    Node_Formals();
    
    ~Node_Formals() = default;
    Node_Formals(Node_Formals&) = delete;
};

class Node_Override : public Generic_Node {
public:
    bool is_override;

/////////// Methods ///////////
    Node_Override(Node_Token* node_override);
    Node_Override();

    Node_Override(Node_Formals&) = delete;
    ~Node_Override() = default;
};


class Node_FuncDecl : public Generic_Node {
public:
/////////// Methods ///////////
    
    Node_FuncDecl(Node_Override* node_override, Node_RetType* node_retType, Node_Token* node_id, Node_Token* node_lparen,
                  Node_Formals* node_formals, Node_Token* node_rparen,
                  Node_Token* node_lbrace,
                  Node_Statement* node_statement, Node_Token* node_rbrace);
    static void newFuncFrame(Node_Override* node_override, Node_RetType* node_retType, Node_Token* node_id, Node_Token* node_lparen,
                      Node_Formals* node_formals, Node_Token* node_rparen);
    
    ~Node_FuncDecl() = default;
    
    Node_FuncDecl(Node_FuncDecl&) = delete;
};

class Node_FuncsList : public Generic_Node {
public:
/////////// Methods ///////////
    Node_FuncsList(NodeVector children): Generic_Node(children){
    
    }
    
    ~Node_FuncsList() = default;
    
    Node_FuncsList(Node_FuncsList&) = delete;
};

class Node_Statement : public Generic_Node {
public:

/////////// Methods ///////////
    Node_Statement(NodeVector children);
    
    ~Node_Statement() = default;
    
    Node_Statement(Node_Statement&) = delete;
    
    
};

class Node_StatementList : public Generic_Node {
public:

/////////// Methods ///////////
    Node_StatementList(NodeVector children);
    
    ~Node_StatementList() = default;
    
    Node_StatementList(Node_StatementList&) = delete;
};

class Node_Exp : public Generic_Node {
public:
    Type type;

/////////// Methods ///////////
    Node_Exp(NodeVector children, Type exp_type) : Generic_Node(children) {
        type = exp_type;
    }
    
    ~Node_Exp() = default;
    
    Node_Exp(Node_Exp&) = delete;
    
    void set_type(Type exp_type){
        type = exp_type;
    }
    
    bool typeCheck(std::vector<Type> type_list){
        for (auto iter = type_list.begin(); iter != type_list.end(); iter++){
            if (type == *iter){
                return true;
            }
        }
        return false;
    }
};

class Node_ExpList : public Generic_Node {
public:
    std::vector<Type> exp_list;
/////////// Methods ///////////
    
    Node_ExpList(Node_Exp* exp);
    Node_ExpList(Node_Exp* node_exp, Node_Token* node_token, Node_ExpList* node_expList);
    
    ~Node_ExpList() = default;
    
    Node_ExpList(Node_ExpList&) = delete;
    
    
};

class Node_Call : public Generic_Node {
public:
    Symbol func_id;
    std::vector<Type> func_parameters;

/////////// Methods ///////////
    Node_Call(Node_Token* node_id, Node_Token* node_lparen, Node_ExpList* node_expList,
              Node_Token* node_rparen);
    Node_Call(Node_Token* node_id, Node_Token* node_lparen, Node_Token* node_rparen);
    
    ~Node_Call() = default;
    
    Node_Call(Node_Call&) = delete;
};

class Node_Exp_Type : public Node_Exp {
public:
    Node_Exp_Type(NodeVector children, Type exp_type) : Node_Exp(children, exp_type) {
    }
    
    ~Node_Exp_Type() = default;
    
    Node_Exp_Type(Node_Exp_Type&) = delete;;
};

class Node_Exp_NUM : public Node_Exp {
public:

/////////// Methods ///////////
    Node_Exp_NUM(NodeVector children, Type exp_type) : Node_Exp(children, exp_type) {
        if (type == Type::BYTE) {
            long token_val = std::atoi(((Node_Token*)(children[0]))->value.c_str());
            
            if (token_val > 255) {
                throw ByteTooLargeExc(yylineno, ((Node_Token*)(children[0]))->value);
            }
        }
        
    }
    
    ~Node_Exp_NUM() = default;
    
    Node_Exp_NUM(Node_Exp_NUM&) = delete;
};

class Node_Exp_Binop : public Node_Exp {
public:

/////////// Methods ///////////
    Node_Exp_Binop(NodeVector children) : Node_Exp(children, Type::INT) {
        //Todo: if bool, update type
        auto exp1 = (Node_Exp*)(children[0]);
        auto binop = (Node_Token*)(children[1]);
        auto exp2 = (Node_Exp*)(children[2]);
        
        if (!exp1->typeCheck({Type::INT, Type::BYTE}) || !exp2->typeCheck({Type::INT, Type::BYTE})) {
            throw MismatchExc(yylineno);
        }
        if (exp1->type == Type::BYTE && exp2->type == Type::BYTE) {
            set_type(Type::BYTE);
        }
        
    }
    
    ~Node_Exp_Binop() = default;
    
    Node_Exp_Binop(Node_Exp_NUM&) = delete;
};

class Node_Exp_Relop : public Node_Exp {
public:

/////////// Methods ///////////
    Node_Exp_Relop(NodeVector children);
    
    ~Node_Exp_Relop() = default;
    
    Node_Exp_Relop(Node_Exp_Relop&) = delete;
};

class Node_Exp_Cast : public Node_Exp {
public:

/////////// Methods ///////////
    Node_Exp_Cast(NodeVector children);
    
    ~Node_Exp_Cast() = default;
    
    Node_Exp_Cast(Node_Exp_Relop&) = delete;
};

class Node_Exp_Str : public Node_Exp {
public:

/////////// Methods ///////////
    Node_Exp_Str(NodeVector children) : Node_Exp(children, Type::STRING) {
    }
    
    ~Node_Exp_Str() = default;
    
    Node_Exp_Str(Node_Exp_Str&) = delete;
};

class Node_Exp_Bool : public Node_Exp {
public:

/////////// Methods ///////////
    Node_Exp_Bool(Node_Token* node_token);
    Node_Exp_Bool(Node_Token* node_not, Node_Exp* node_exp);
    Node_Exp_Bool(Node_Exp* node_exp1, Node_Token* node_AndOR, Node_Exp* node_exp2);
    Node_Exp_Bool(Node_Exp* node_exp);
    ~Node_Exp_Bool() = default;
    
    Node_Exp_Bool(Node_Exp_Bool&) = delete;
};

class Node_Exp_ID : public Node_Exp {
public:
    Symbol id;
    
    Node_Exp_ID(Node_Token* node_token);
    ~Node_Exp_ID() = default;
    Node_Exp_ID(Node_Exp_ID&) = delete;
};

class Node_Exp_Call : public Node_Exp {
public:
    
    Node_Exp_Call(NodeVector children) : Node_Exp(children, Type::INVALID) {
        auto node_call = (Node_Call*)(children[0]);

        
        set_type(node_call->func_id.type);
    }
    
    ~Node_Exp_Call() = default;
    
    Node_Exp_Call(Node_Exp_Call&) = delete;;
};

class Node_Statement_Block : public Node_Statement {
public:
/////////// Methods ///////////
    Node_Statement_Block(NodeVector children);
    
    ~Node_Statement_Block() = default;
    
    Node_Statement_Block(Node_Statement_Block&) = delete;
};

class Node_Statement_ID_Decl : public Node_Statement {
public:

/////////// Methods ///////////
    Node_Statement_ID_Decl(Node_Exp*node_type, Node_Token* node_token,
                           Node_Token* node_sc);
    
    Node_Statement_ID_Decl(Node_Exp* node_type, Node_Token* node_token,
                           Node_Token* node_assign,
                           Node_Exp* node_exp, Node_Token* node_sc);
    
    ~Node_Statement_ID_Decl() = default;
    
    Node_Statement_ID_Decl(Node_Statement_ID_Decl&) = delete;
};

class Node_Statement_ID_Assign : public Node_Statement {
public:

/////////// Methods ///////////
    Node_Statement_ID_Assign(Node_Token* node_id, Node_Token* node_assign, Node_Exp* node_exp,
                             Node_Token* node_sc);
    
    ~Node_Statement_ID_Assign() = default;
    
    Node_Statement_ID_Assign(Node_Statement_ID_Assign&) = delete;
};

class Node_Statement_Call : public Node_Statement {
public:

/////////// Methods ///////////
    Node_Statement_Call(Node_Call* node_call, Node_Token* node_sc);
    
    ~Node_Statement_Call() = default;
    
    Node_Statement_Call(Node_Statement_Call&) = delete;
};

class Node_Statement_Ret : public Node_Statement {
public:
/////////// Methods ///////////
    Node_Statement_Ret(Node_Token* node_ret, Node_Token* node_sc);
    
    Node_Statement_Ret(Node_Token* node_ret, Node_Exp* node_exp, Node_Token* node_sc);
    
    ~Node_Statement_Ret() = default;
    
    Node_Statement_Ret(Node_Statement_Ret&) = delete;
};

class Node_Statement_IF : public Node_Statement {
public:
/////////// Methods ///////////
    Node_Statement_IF(Node_Token* node_if, Node_Token* node_lparen, Node_Exp* node_exp,
                      Node_Token* node_rparen,
                      Node_Statement* node_statement);
    
    Node_Statement_IF(Node_Token* node_if, Node_Token* node_lparen, Node_Exp* node_exp,
                      Node_Token* node_rparen,
                      Node_Statement* node_statement1, Node_Token* node_else,
                      Node_Statement* node_statement2);
    
    ~Node_Statement_IF() = default;
    
    Node_Statement_IF(Node_Statement_IF&) = delete;
};

class Node_Statement_While : public Node_Statement {
public:
/////////// Methods ///////////
    Node_Statement_While(Node_Token* node_while, Node_Token* node_lparen, Node_Exp* node_exp,
                         Node_Token* node_rparen, Node_Statement* node_statement);
    
    ~Node_Statement_While() = default;
    
    Node_Statement_While(Node_Statement_While&) = delete;
};

class Node_Statement_LoopMod : public Node_Statement {
public:
/////////// Methods ///////////
    Node_Statement_LoopMod(Node_Token* node_loop_mod, Node_Token* node_sc);
    
    ~Node_Statement_LoopMod() = default;
    
    Node_Statement_LoopMod(Node_Statement_LoopMod&) = delete;
};

//#define YYSTYPE Node
    

/*
struct YYSTYPE {
    Generic_Node * ProgramNode;
    Node_Token * NodeToken;
    Node_RetType * NodeRetType;
    Node_FormalDecl * NodeFormalDecl;
    Node_FormalsList * NodeFormalsList;
    Node_FuncDecl * NodeFuncDecl;
    Node_FuncsList * NodeFuncsList;
    Node_Statement * NodeStatement;
    Node_StatementList * NodeStatementList;
    Node_Exp * NodeExp;
    Node_ExpList * NodeExpList;
    Node_Call * NodeCall;
};
*/

#endif