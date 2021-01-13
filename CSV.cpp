#include <vector>
#include <map>
#include <cstdlib>
#include <string>
#include <iostream>
#include <fstream>
#include <iomanip>
using namespace std;

bool debug = false;

struct Element{
public:
    bool isNum(){
        if(t == "int") return true;
        for(int i = 0; i < v.size(); i++)
            if(v[i] != '-' && (v[i] < '0' || v[i] > '9'))
                return false;
        return true;
    }

    void setVal(string s){
        v = s;
        if(isNum()) t = "int";
        else t = t == "null" ? "null" : "string";
    }

    void setVal(int i){
        t = "int";
        v = to_string(i);
    }

    string type(){
        return t;
    }

    string& val(){
        //if(t == "string") return string("\"" + v + "\"");
        return v;
    }

    int iVal(){
        try{
            if(t != "int")
                throw v + string("(type = ") + string(t + ")") + string("cannot convert to int!");
        } catch(string errmsg){
            if(debug) cout << errmsg << endl;
        }
        return atoi(v.c_str());
    }

    Element(string s = "null") : v(s){
        if(isNum()) t = "int";
        else t = v == "null" ? "null" : "string";
    }

    Element(int i) : v(to_string(i)){
        t = "int";
    }

private:
    string t;
    string v;
};

vector<string> strSplit(string s, string delimiter){
	vector<string> ans;
	if(delimiter.length() > s.length()) return ans;
	int l = 0;
	for(int i = 0; i < s.length(); ){
		if(s.substr(i, delimiter.length()) == delimiter){
			ans.push_back(s.substr(l, i - l));
			i = i + delimiter.length();
			l = i;
		} else i++;
	}
	if(l < s.length() && s.substr(l) != "") ans.push_back(s.substr(l));
	return ans;
}

bool isNum(string v){
    for(int i = 0; i < v.size(); i++)
        if(v[i] != '-' && v[i] < '0' || v[i] > '9')
            return false;
    return true;
}

string typeOf(string s){
    
    if(s[0] == '\"' && s[s.size() - 1] == '\"') return "string";
    if(isNum(s) || s == "False" || s == "True") return "int";
    return "null"; 
}

class Excel{
public:
    Excel();
    void addCol(string cname);
    void addIndex(string iname);
    void addElement(string col, string index);
    void valUpdate(int col, int index);
    Element& operator()(string c, string i);
    Element& operator()(int c, int i);
    friend vector<string> strSplit(string s, string delimiter);
    friend vector<string> funcParse(const string s);
    friend vector<string> argParse(const string s, const string d);
    friend vector<string> posParse(const string s);
    string valParse(const string s);
    string SUM(string arg);
    string IF(string arg);
    string VLOOKUP(string arg);
    void print(int w = 12);
    void printCSV();

private:
    vector<int> targetPos;
    map< int, map< int, Element> > table;
    map<string, int> C;
    map<int, string> indexC;
    map<string, int> I;
    map<int, string> indexI;
};


Excel::Excel(){
    table[1][1] = Element();
    targetPos.push_back(0); // c
    targetPos.push_back(0); // i
}

void Excel::addCol(string cname){
    indexC[C.size() + 1] = cname;
    C[cname] = C.size() + 1;
    for(int i = 1; i < I.size(); i++)
        table[C[cname]][i] = Element();
}

void Excel::addIndex(string iname){
    indexI[I.size() + 1] = iname;
    I[iname] = I.size() + 1;
    for(int i = 1; i < C.size(); i++)
        table[i][I[iname]] = Element();
}


void Excel::valUpdate(int col, int index){

    try{
        if(col > C.size() || col < 1)
            throw string("index error : col = " + to_string(col));
        if(index > I.size() || index < 1)
            throw string("index error : index = " + to_string(index));
    } catch(string errmsg){
        if(debug) cout << errmsg;
        exit(0);
    }

    targetPos[0] = col;
    targetPos[1] = index;

    table[col][index].val() = valParse(table[col][index].val());
}

Element& Excel::operator()(string col, string index){
    try{
        if(C.find(col) == C.end())
            throw string("index error : col = " + col);
        if(I.find(index) == I.end())
            throw string("index error : index = " + index);
        for(int i = 0; i < col.size(); i++)
            if(col[i] < 'A' || col[i] > 'Z')
                throw string("col name should be capital, you pass : ") + col;
        for(int i = 0; i < index.size(); i++)
            if(index[i] < 'a' || index[i] > 'z')
                throw string("index name should be lower case, you pass : ") + index;
    } catch(string errmsg){
        if(debug) cout << errmsg;
        exit(0);
    }
    return table[C[col]][I[index]];
}

Element& Excel::operator()(int col, int index){
    try{
        if(col > C.size() || col < 1)
            throw string("index error : col = " + to_string(col));
        if(index > I.size() || index < 1)
            throw string("index error : index = " + to_string(index));
    } catch(string errmsg){
        if(debug) cout << errmsg;
        exit(0);
    }

    return table[col][index];
}

vector<string> argParse(const string s, const string d){
    vector<string> args;
    if(typeOf(s) == "string"){
        args.push_back(s);
        return args;
    } 
    string arg;
    bool pflag, dflag;
    int pcnt;
    //split by ',' omit ' ', parse ()

    for(int i = 0; i < s.size(); i++){
        arg = "";
        dflag = false;
        pflag = false;
        pcnt = 0;

        while(i < s.size()){
            
            if(s[i] != ' '){
                if(s[i] == '\"' ){
                    dflag = !dflag;
                }

                if(!dflag){
                    if((!pflag && s.substr(i, d.size()) == d) || (pflag && pcnt == 0 && s.substr(i, d.size()) == d)){
                        i += (d.size() - 1);
                        break;
                    }  
                }

                arg += s[i];

                if(!dflag && s[i] == '('){
                    pflag = true;
                    pcnt++;
                } else if(!dflag && s[i] == ')'){
                    if(pcnt == 0 || pflag == false){
                        if(debug) cout << "\")\" should appear after \"(\"" << endl;
                        break;
                    }
                    pcnt--;
                }
            }

            i++;
        }

        args.push_back(arg);
    }
    return args;
}

vector<string> funcParse(const string s){
    string str = "";
    vector<string> ans;

    for(int i = 0; i < s.size(); i++)
        if(s[i] != ' ')
            str += s[i];
    
    if(str[str.size() - 1] != ')' || typeOf(str) == "string"){
        ans.push_back(s);
        return ans;
    }
    //split by ',' omit ' ', parse ()

    if(str.size() >= 4 && str.substr(0, 2) == "IF"){
        ans.push_back(str.substr(0, 2));
        ans.push_back(str.substr(3, str.size() - 4));
    } else if(str.size() >= 5 && str.substr(0, 3) == "SUM"){
        ans.push_back(str.substr(0, 3));
        ans.push_back(str.substr(4, str.size() - 5));
    } else if(str.size() >= 9 && str.substr(0, 7) == "VLOOKUP"){
        ans.push_back(str.substr(0, 7));
        ans.push_back(str.substr(8, str.size() - 9));
    } else {
        ans.push_back(str);
    }
    return ans;
}

vector<string> posParse(const string s){
    vector<string> res;
    string arg1 = "", arg2 = "";
    bool capitalOff = false;
    for(int i = 0; i < s.size(); i++){
        try{
            if(s[i] <= 'Z' && s[i] >= 'A'){
                arg1 += s[i];
                capitalOff = true;
            } else if(s[i] <= 'z' && s[i] >= 'a'){
                if(capitalOff)
                    arg2 += s[i];
                //else throw string("format error : column name first\n");
            } //else throw string(string("error character : (") + s[i] + string(")\n"));
        } catch(const string errmsg){
            if(debug) cout << errmsg;
            res.push_back("null");
            res.push_back("null");
            return res;
        }
    }
    res.push_back(arg1);
    res.push_back(arg2);
    return res;
}

string Excel::valParse(const string s){
    if(isNum(s) || typeOf(s) == "string") return s;

    string op, res;
    vector<string> pos, cpArg, funcArg;

    op = "null";
    res = s;

    if(argParse(res, ">=").size() > 1)
        op = ">=";
    else if(argParse(res, "<=").size() > 1)
        op = "<=";
    else if(argParse(res, "=").size() > 1)
        op = "=";
    else if(argParse(res, ">").size() > 1)
        op = ">";
    else if(argParse(res, "<").size() > 1)
        op = "<";

    if(op != "null"){
        vector<string> lval, rval;
        cpArg = argParse(res, op);

        if(cpArg.size() != 2){
            if(debug) cout << "size of cpArg in valParse error : " << cpArg.size() << endl;
            return "null"; 
        }

        lval = funcParse(cpArg[0]);
        if(lval.size() > 1){
            if(lval[0] == "SUM")
                cpArg[0] = SUM(lval[1]);
            else if(lval[0] == "IF")
                cpArg[0] = IF(lval[1]);
            else if(lval[0] == "VLOOKUP")
                cpArg[0] = VLOOKUP(lval[1]);
            else {
                res = "null";
            }

            if(cpArg[0] != "null" && !isNum(cpArg[0]) && typeOf(cpArg[0]) != "string" && strSplit(cpArg[0], ":").size() == 1){
                pos = posParse(cpArg[0]);
                if(C[pos[0]] == targetPos[0] && I[pos[1]] == targetPos[1]) return "null";
                cpArg[0] = table[C[pos[0]]][I[pos[1]]].val();

                if(funcParse(cpArg[0]).size() > 1)
                    cpArg[0] = valParse(cpArg[0]);
                else if(argParse(cpArg[0], ">=").size() > 1 ||
                    argParse(cpArg[0], "<=").size() > 1 ||
                    argParse(cpArg[0], "=").size() > 1 ||
                    argParse(cpArg[0], ">").size() > 1 ||
                    argParse(cpArg[0], "<").size() > 1)
                    cpArg[0] = valParse(cpArg[0]);
            }
        } else {
            if(cpArg[0] != "null" && !isNum(cpArg[0]) && typeOf(cpArg[0]) != "string" && strSplit(cpArg[0], ":").size() == 1){
                pos = posParse(cpArg[0]);
                if(C[pos[0]] == targetPos[0] && I[pos[1]] == targetPos[1]) return "null";
                cpArg[0] = table[C[pos[0]]][I[pos[1]]].val();

                if(funcParse(cpArg[0]).size() > 1)
                    cpArg[0] = valParse(cpArg[0]);
                else if(argParse(cpArg[0], ">=").size() > 1 ||
                    argParse(cpArg[0], "<=").size() > 1 ||
                    argParse(cpArg[0], "=").size() > 1 ||
                    argParse(cpArg[0], ">").size() > 1 ||
                    argParse(cpArg[0], "<").size() > 1)
                    cpArg[0] = valParse(cpArg[0]);
            }
        }

        rval = funcParse(cpArg[1]);
        if(rval.size() > 1){
            if(rval[0] == "SUM")
                cpArg[1] = SUM(rval[1]);
            else if(rval[0] == "IF")
                cpArg[1] = IF(rval[1]);
            else if(rval[0] == "VLOOKUP")
                cpArg[1] = VLOOKUP(rval[1]);
            else {
                res = "null";
            }

            if(cpArg[1] != "null" && !isNum(cpArg[1]) && typeOf(cpArg[1]) != "string" && strSplit(cpArg[1], ":").size() == 1){
                pos = posParse(cpArg[1]);
                if(C[pos[0]] == targetPos[0] && I[pos[1]] == targetPos[1]) return "null";
                cpArg[1] = table[C[pos[0]]][I[pos[1]]].val();

                if(funcParse(cpArg[1]).size() > 1)
                    cpArg[1] = valParse(cpArg[1]);
                else if(argParse(cpArg[1], ">=").size() > 1 ||
                    argParse(cpArg[1], "<=").size() > 1 ||
                    argParse(cpArg[1], "=").size() > 1 ||
                    argParse(cpArg[1], ">").size() > 1 ||
                    argParse(cpArg[1], "<").size() > 1)
                    cpArg[1] = valParse(cpArg[1]);
            }
        } else {
            if(cpArg[1] != "null" && !isNum(cpArg[1]) && typeOf(cpArg[1]) != "string" && strSplit(cpArg[1], ":").size() == 1){
                pos = posParse(cpArg[1]);
                if(C[pos[0]] == targetPos[0] && I[pos[1]] == targetPos[1]) return "null";
                cpArg[1] = table[C[pos[0]]][I[pos[1]]].val();

                if(funcParse(cpArg[1]).size() > 1)
                    cpArg[1] = valParse(cpArg[1]);
                else if(argParse(cpArg[1], ">=").size() > 1 ||
                    argParse(cpArg[1], "<=").size() > 1 ||
                    argParse(cpArg[1], "=").size() > 1 ||
                    argParse(cpArg[1], ">").size() > 1 ||
                    argParse(cpArg[1], "<").size() > 1)
                    cpArg[1] = valParse(cpArg[1]);
            }
        }

        if(debug) cout << ">>>>" << cpArg[0] << " " << cpArg[1] << endl;

        if(cpArg[0] == "null" || cpArg[1] == "null"){
            return "null";
        }

        if(typeOf(cpArg[0]) != typeOf(cpArg[1])){
            if(debug) cout << "type inconsistent : lval = " << cpArg[0] << ", rval = " << cpArg[1] << endl;
            return "0";
        }

        if(op == ">"){
            if(atoi(cpArg[0].c_str()) > atoi(cpArg[1].c_str()))
                res = "1";
            else res = "0";
        } else if(op == "<"){
            if(atoi(cpArg[0].c_str()) < atoi(cpArg[1].c_str()))
                res = "1";
            else res = "0";
        } else if(op == "="){
            if(cpArg[0] == cpArg[1])
                res = "1";
            else res = "0";
        } else if(op == ">="){
            if(atoi(cpArg[0].c_str()) >= atoi(cpArg[1].c_str()))
                res = "1";
            else res = "0";
        } else if(op == "<="){
            if(atoi(cpArg[0].c_str()) <= atoi(cpArg[1].c_str()))
                res = "1";
            else res = "0";
        } 

    } else {
        vector<string> val;
        
        val = funcParse(s);
        
        if(val.size() > 1){
            if(val[0] == "SUM")
                res = SUM(val[1]);
            else if(val[0] == "IF")
                res = IF(val[1]);
            else if(val[0] == "VLOOKUP")
                res = VLOOKUP(val[1]);
            else {
                if(debug) cout << "val error in valParse" << endl;
                res = "null";
            }

            if(res != "null" && !isNum(res) && typeOf(res) != "string" && strSplit(res, ":").size() == 1){
                pos = posParse(res);
                if(C[pos[0]] == targetPos[0] && I[pos[1]] == targetPos[1]) return "null";
                res = table[C[pos[0]]][I[pos[1]]].val();

                if(funcParse(res).size() > 1)
                    res = valParse(res);
                else if(argParse(res, ">=").size() > 1 ||
                    argParse(res, "<=").size() > 1 ||
                    argParse(res, "=").size() > 1 ||
                    argParse(res, ">").size() > 1 ||
                    argParse(res, "<").size() > 1)
                    res = valParse(res);
            }
        } else {


            if(res == "False") res = "0";
            if(res == "True") res = "1";

            if(res != "null" && !isNum(res) && strSplit(res, ":").size() == 1){
                pos = posParse(res);
                if(C[pos[0]] == targetPos[0] && I[pos[1]] == targetPos[1]) return "null";
                res = table[C[pos[0]]][I[pos[1]]].val();

                if(funcParse(res).size() > 1)
                    res = valParse(res);
                else if(argParse(res, ">=").size() > 1 ||
                    argParse(res, "<=").size() > 1 ||
                    argParse(res, "=").size() > 1 ||
                    argParse(res, ">").size() > 1 ||
                    argParse(res, "<").size() > 1)
                    res = valParse(res);
            }
        }
    }
    if(debug) cout << "res : " << res << endl;
    return res;
}

string Excel::SUM(string arg){
    vector<string> funcArg, rangeArg, args;

    args = argParse(arg, ",");

    int sum = 0;
    vector<string> pos1, pos2;

    for(int i = 0; i < args.size(); i++){
        args[i] = valParse(args[i]);
        if(debug) cout << "sum arg : " << i << " " << args[i] << endl;
    }
    
    for(int i = 0; i < args.size(); i++){
        rangeArg = strSplit(args[i], ":");

        //case1 1 num
        if(rangeArg.size() == 1){
            if(isNum(rangeArg[0]))
                sum += atoi(rangeArg[0].c_str());
            else{
                pos1 = posParse(rangeArg[0]);
                if(table[C[pos1[0]]][I[pos1[1]]].val() == "null")
                    return "null";
                table[C[pos1[0]]][I[pos1[1]]].val() = valParse(table[C[pos1[0]]][I[pos1[1]]].val());
                sum += table[C[pos1[0]]][I[pos1[1]]].iVal();
            }
        }
        
        //case2 multiple num
        if(rangeArg.size() == 2){
            if(debug) cout << "case 2 : " << rangeArg[0] << " " << rangeArg[1] << endl;
            pos1 = posParse(rangeArg[0]);
            pos2 = posParse(rangeArg[1]);
            if(debug) cout << "(" << pos1[0] << "," << pos1[1] << ")->(" << pos2[0] << "," << pos2[1] << ")" << endl;
            for(int i = I[pos1[1]]; i <= I[pos2[1]]; i++)
                for(int c = C[pos1[0]]; c <= C[pos2[0]]; c++){
                    if(c == targetPos[0] && i == targetPos[1])
                        return "null";
                    if(table[c][i].val() == "null")
                        return "null";
                    table[c][i].val() = valParse(table[c][i].val());
                    sum += table[c][i].iVal();
                }
        }
    }

    return to_string(sum);
}

string Excel::IF(string arg){
    vector<string> funcArg, cpArg, args;
    string res, cpOp = "none";

    args = argParse(arg, ",");


    if(args.size() == 3){
        for(int i = 0; i < args.size(); i++){
            args[i] = valParse(args[i]);
            if(debug) cout << "if arg : " << i << " " << args[i] << endl;
        }

        if(strSplit(args[0], ":").size() == 1){
            res = args[0] == "1" ? args[1] : (args[0] == "0" ? args[2] : "null");
            if(debug) cout << "if res = " << res << endl;
        } else {
            if(debug) cout << "the \":\" shouldn't appear in first arg of IF" << endl;
            res = "null";
        }
    } else {
        if(debug) cout << "num of arg in IF is not correct, expect 3 given, get " << args.size() << endl;
        res = "null";
    }

    return res;
}

string Excel::VLOOKUP(string arg){
    vector<string> args, rangeArg, pos;
    string res = "null";

    args = argParse(arg, ",");

    if(args.size() == 4){
        for(int i = 0; i < 4; i++){
            args[i] = valParse(args[i]);
            if(debug) cout << "vlookup arg " << i << " = " << args[i] << endl;
        }

        if(typeOf(args[2]) == "int" && args[3] == "0"){
            rangeArg = strSplit(args[1], ":");

            if(rangeArg.size() == 2){
                vector<string> pos1, pos2;
                int diff = atoi(args[2].c_str()) - 1;
                pos1 = posParse(rangeArg[0]);
                pos2 = posParse(rangeArg[1]);
                if(C[pos1[0]] + diff <= C[pos2[0]]){
                    for(int i = I[pos1[1]]; i <= I[pos2[1]]; i++){
                        if(C[pos1[0]] == targetPos[0] && i == targetPos[1])
                            return "null";
                        if(valParse(table[C[pos1[0]]][i].val()) == args[0]){
                            if(C[pos1[0]] + diff == targetPos[0] && i == targetPos[1])
                                return "null";
                            res = valParse(table[C[pos1[0]] + diff][i].val());
                            break;
                        }
                    }
                }
            } else if(rangeArg.size() == 1 && args[2] == "1"){
                vector<string> pos;
                pos = posParse(args[1]);
                if(C[pos[0]] == targetPos[0] && I[pos[1]] == targetPos[1])
                    return "null";
                res = valParse(table[C[pos[0]]][I[pos[1]]].val());
            } else {
                if(debug) cout << "val type wrong in VLOOKUP" << endl;
                return "null";
            }
        } else {
            if(debug) cout << "something error in VLOOKUP" << endl;
            for(int i = 0; i < 4; i++)
                if(debug) cout << "vlookup arg : " << i << " " << args[i] << endl;
            return "null";
        }

    } else {
        if(debug) cout << "num of arg should be 4 in VLOOKUP, get :" << args.size() << endl;
        return "null";
    }

    return res;
}

void Excel::printCSV(){

    for(int c = 1; c <= indexC.size(); c++)
        cout << "," << indexC[c];
    cout << endl;

    for(int i = 1; i <= indexI.size(); i++){
        cout << indexI[i];
        for(int c = 1; c <= indexC.size(); c++)
            cout << "," << table[c][i].val();
        cout << endl;
    }
}

void Excel::print(int w){
    for(int i = 0; i < C.size() + 1; i++)
        for(int j = 0; j < w + 1; j++)
            cout << "-";
    cout << "-" << endl << "|" << setw(w) << "";

    for(int c = 1; c <= indexC.size(); c++)
        cout << "|" << left << setw(w) << indexC[c];
    cout << "|" << endl;

    for(int i = 0; i < C.size() + 1; i++)
        for(int j = 0; j < w + 1; j++)
            cout << "-";
    cout << "-" << endl;

    for(int i = 1; i <= indexI.size(); i++){
        cout << "|" << left << setw(w) << indexI[i];
        for(int c = 1; c <= indexC.size(); c++)
            cout << "|" << left << setw(w) << table[c][i].val();
        cout << "|" << endl;

        for(int i = 0; i < C.size() + 1; i++)
            for(int j = 0; j < w + 1; j++)
                cout << "-";
         cout << "-" << endl;

    }
    cout << endl;
}


int main(int argc, char * argv[]){

    ifstream fin;
    fin.open(argv[1], ios::in);
    vector< vector<string> > csvInput;

    string buf;
    while(getline(fin, buf)){
        csvInput.push_back(  argParse(buf, ",") );
    }
    
    /*
    for(int i = 0; i < csvInput.size(); i++){
        for(int c = 0; c < csvInput[0].size(); c++)
            cout << left << setw(12) << csvInput[i][c] << " ";
        cout << endl;
    }*/

    Excel e;

    if(csvInput.size() > 0){
        for(int c = 1; c < csvInput[0].size(); c++)
            e.addCol(csvInput[0][c]);
        for(int i = 1; i < csvInput.size(); i++)
            e.addIndex(csvInput[i][0]);
        for(int i = 1; i < csvInput.size(); i++)
            for(int c = 1; c < csvInput[0].size(); c++)
                e(c, i) = csvInput[i][c];

        for(int i = 1; i < csvInput.size(); i++)
            for(int c = 1; c < csvInput[0].size(); c++){
                if(debug) cout << "i = " << i << " c = " << c << " " << csvInput[i][c] << endl; 
                e.valUpdate(c, i);
                //e(c, i) = e.valParse(e(c, i).val());
            }
    }

    e.printCSV();

    //e.print();

    /*
    vector<string> args;
    string arg;
    do{
        cout << ">";
        getline(cin, arg);
        args = funcParse(arg);
        if(args.size() == 2){
            if(args[0] == "SUM")
                cout << args[0] << "(" << args[1] << ") = \n" << e.SUM(args[1]) << endl;
            else if(args[0] == "IF")
                cout << args[0] << "(" << args[1] << ") = \n" << e.IF(args[1]) << endl;
            else if(args[0] == "VLOOKUP")
                cout << args[0] << "(" << args[1] << ") = \n" << e.VLOOKUP(args[1]) << endl;
        }
    } while(arg != "exit");*/
    
    return 0;
}