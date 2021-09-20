#include <iostream>
#include <vector>
#include <string>
#include <iomanip>

using namespace std;

string variables = "abcd";
vector<vector<string>> matrixScheme {
    {"2a + 4b", "2a - 2b", "2a - 2b"},
    {"2a - 2b", "2a + b + 3c", "2a + b - 3c"},
    {"2a - 2b", "2a + b - 3c", "2a + b + 3c"}
};

vector<string> vectorScheme {
    "-4a - 2b",
    "-4a + b + 9c",
    "-4a + b - 9c"
};

bool isOperator(const string& str){
    //оператор
    if(str.find_first_not_of("-+") == std::string::npos){
        return true;
    }
    return false;
}

int whatIsIt(const string& str){
    //число и переменные
    if(str.find_first_not_of("1234567890" + variables) == std::string::npos){
        return 1;
    }
    //отрицательное число и переменные
    if(str.find_first_not_of("-1234567890" + variables) == std::string::npos){
        return 5;
    }
    //число
    if(str.find_first_not_of("1234567890") == std::string::npos){
        return 2;
    }
    //переменная
    if(str.find_first_not_of(variables) == std::string::npos){
        return 3;
    }
    return 0;
}

string getExp(const vector<double>& lambda, const string& scheme, string exp){
    int what = whatIsIt(exp);

    if(isOperator(exp)){
        return exp;
    }
    if(what == 1 || what == 5){
        //получаем число, записываем его в определённую переменную
        if(exp.size() == 1){
            return to_string(lambda[variables.find(exp)]);
        }
        int index = exp.find_first_of(variables);
        string str = exp.substr(0U, index);
        double  number = stod(exp.substr(0U, index));
        double var = lambda[variables.find(exp.substr(index, exp.length()))];
        exp = to_string(number * var);
    }
    if(what == 3) {
        exp = to_string(lambda[variables.find(exp)]);
    }

    return exp;
}

vector<string> getExpressions(const vector<double>& lambda,const string& scheme){
    string delimiter = " ";
    vector<string> exps;
    auto start = 0U;
    auto end = scheme.find(delimiter);
    while (end != std::string::npos) {
        string exp = getExp(lambda, scheme, scheme.substr(start, end - start));
        //проверяю, есть ли в выражении переменная
        start = end + delimiter.length();
        end = scheme.find(delimiter, start);
        exps.push_back(exp);
    }
    string exp = getExp(lambda, scheme, scheme.substr(start, end - start));
    exps.push_back(exp);
    return exps;
}

double solveExpression(double left,  const string& operation, const string& right){

    return operation == "-" ? left - stod(right) : left + stod(right);
}


double calculate(const vector<double>& lambda,const string& scheme) {
    vector<string> exps = getExpressions(lambda,scheme);
    double calculated = stod(exps[0]);
    for(int i = 1; i < exps.size() - 1; i+=2){
        calculated = solveExpression(calculated, exps[i], exps[i+1]);
    }
    return calculated;
}


class LDL {
private:
    vector<vector<double>> matrix;
    vector<double> b;
    int size;
public:
    LDL(const vector<double>& lambda, int size) : size(size) {
        for(int i = 0; i < size; i++){
            vector<double> mat;
            for(int j = 0; j < size; j++){
                mat.push_back(calculate(lambda, matrixScheme[i][j]));
            }
            matrix.push_back(mat);
            b.push_back(calculate(lambda, vectorScheme[i]));
        }
    }
    friend ostream& operator<<(ostream& out, const LDL& ldl){
        for(int i = 0; i < ldl.size; i++){
            for(int j = 0; j < ldl.size; j++) {
                out << ldl.matrix[i][j] << setw(12);
            }
            out << setw(12) << "|" << ldl.b[i] << "\n";
        }
        return out;
    }
    void decomp(){
        for(int i = 0; i < size; i++){
            for(int j = i; j < size; j++){
                double sum = matrix[j][i];
                for(int k = 0; k < i; k++) sum -= matrix[i][k] * matrix[k][k] * matrix[j][k];
                if(i == j) {
                    matrix[i][i] = sum;
                } else {
                    matrix[j][i] = sum / matrix[i][i];
                    matrix[i][j] = matrix[j][i];
                }
            }

        }
    }
    vector<double> solving() {
        vector<double> c(size);
        vector<double> x(size);
        cout << "c': " << "\n";
        for (int i = 0; i < size; i++) {
            c[i] = b[i];
            double bi = b[i];
            int k = 0;
            for (int j = 0; j < size; j++) {
                if(i > j) {
                    c[i] -= matrix[i][j]*c[k];
                    double m = matrix[i][j]*c[k];
                    double cm = c[k];
                    m = m;
                    k++;
                }
            }
            cout << c[i] << setw(12);
        }
        for(int f = size - 1; f >= 0; f--) {
            x[f] = c[f]/matrix[f][f];
            double xf = x[f];
            int k = size - 1;
            for(int g = size - 1; g >= 0; g--) {
                if(f < g) {
                    x[f] -= matrix[f][g]*x[k];
                    k--;
                }
            }
            xf = x[f];
        }
        return x;
    }

};

int main() {

    int size = 3;
    vector<double> lambda(size);
    /*cout << "*Enter size: ";
    cin >> size;*/
    cout << "\n* Enter λ[1 - " << size << "]" << "\n";
    for(int i = 0; i < size; i++) {
        cout << "\tλ" << i + 1 << ":";
        cin >> lambda[i];
    }
    LDL ldl(lambda, 3);
    cout << "\n" << ldl;
    ldl.decomp();
    cout << "\nDecompiled\n" << ldl << "\n";
    vector<double> x = ldl.solving();
    cout << "\nX: \n";
    for(double i : x) {
        cout << i << setw(12);
    }
    return 0;
}