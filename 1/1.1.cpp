#include <iostream>
#include <vector>
#include <iomanip>
#include <fstream>
#include <string>

using std::vector;
using std::string;
using std::cout;
using std::begin;
using std::cin;

class IER : public std::exception {
    const int m_msg;
public:
    explicit IER(const int& msg) : m_msg(msg){}

    [[nodiscard]] const char * what() const override {
        return reinterpret_cast<const char *>(m_msg);
    }
};

class Gauss {
private:
    vector<vector<double>> kf;
    vector<double> _v;
    vector<double> x;
    vector<vector<double>> save_kf;
    vector<double> save_v;
    int size;
public:
    //col - row в каком столбце и от какой строки
    double findMaxAndReplace(int col, int row){
        double max = kf[col][row];
        int fRow = NULL;
        for(int i = row; i < kf.size(); i++){
            if(abs(kf[i][col]) > abs(max)) {
                max = kf[i][col];
                fRow = i;
            }
        }
        if(max == 0){
            throw IER(1);
        }
        if(fRow != row){
            std::swap(kf[fRow], kf[row]);
            std::swap(_v[row], _v[fRow]);
        }
        return max;
    }
    //исключение переменной
    void excludeVar(int col, int row, double max) {
        _v[col] /= max;
        for(int i = col; i < kf[row].size(); i++){
            kf[row][i] /=  max;

        }
        for(int i = row + 1; i < kf.size(); i++){
            double k = kf[i][col]/kf[row][col];
            for(int j = 0; j < kf[i].size(); j++){
                kf[i][j] -= kf[row][j]*k;
            }
            _v[i] -= _v[row]*k;
        }
    }
    Gauss(int size, const vector<double>& elements, const vector<double>& _vec): size(size){
        //заполнение матрицы
        int i = 0;
        x.resize(size);
        memset(x.data(), 1, sizeof(double)*size);
        for(int j = 0; j < size; j++, i+=size){
            //копируем от j * size size элементов
            vector<double> row;
            row.assign(begin(elements)+j*size, begin(elements)+(j+1)*size);
            kf.push_back(row);
            save_kf.push_back(row);
        }
        _v = _vec;
        save_v = _vec;
    }
    double uncertainty(){
        vector<double> res = residual();
        Gauss slae(size, save_kf, res);
        vector<double> x2 = slae.solve();
        //вычисление нормы
        double max = x2[0] - res[0];
        for(int i = 0; i < size; i++){
            if(abs(max) > abs(x2[i] - res[i])){
                max = x2[i] - res[i];
            }
        }
        double unc = max/norm();
        return unc;
    }
    Gauss(int size, const vector<vector<double>>& elements, const vector<double>& _vec)
    :kf(elements), save_kf(elements), _v(_vec), save_v(_vec), size(size){
        x.resize(size);
        memset(x.data(), 1, sizeof(double)*size);

    }
    //обратный ход
    void reverse(){
        for(int i = kf.size() - 1; i >= 0; i--) {
            double xTemp = _v[i];
            for(int j = i + 1; j < kf[i].size(); j++){
                xTemp -= kf[i][j] * x[j];
            }
            xTemp /= kf[i][i];
            x[i] = xTemp;
        }
    }
    void print() const {
        cout << "\n==================================\n";
        for(int i = 0; i < kf.size(); i++){
            for(const auto& elem: kf[i]){
                cout << elem << std::setw(16);
            }
            cout << std::setw(6) << "|" << _v[i];
            cout << "\n";
        }

        cout << "==================================\n";
    }

    vector<double> residual() {
        vector<double> ax = getAX();
        vector<double> residual;
        residual.reserve(ax.size());
        for(int i = 0; i < ax.size(); i++){
            residual.push_back(ax[i] - save_v[i]);
        }
        return residual;
    }
    vector<double> getAX(){
        vector<double> ax;
        for(int i = 0; i < save_kf.size(); i++){
            double  temp = 0;
            for(int j = 0; j < save_kf[i].size(); j++){
                temp += save_kf[i][j]*x[i];
            }
            ax.push_back(temp);
        }
        return ax;
    }
    double norm(){
        //найти макимальное число в векторе.
        vector<double> res = residual();
        double max = res[0];
        for(int i = 0; i < size; i++){
            if(abs(max) > abs(res[i])){
                max = res[i];
            }
        }
        return max;
    }
    //прямой ход
    void straight() {
        for (int i = 0; i < size - 1; i++) {
            double max = findMaxAndReplace(i, i);
            excludeVar(i, i, max);
        }
    }
    vector<double> solve() {
        straight();
        reverse();
        return x;
    }
};

class GaussFactory{
    vector<Gauss> slaes;
public:
    explicit GaussFactory(const string& path){
        //читаем файл
        std::ifstream file;
        file.open(path);

        if(file.is_open()){
            while(!file.eof()) {
                //читаем порядок матрицы (подрузомевается, что матрица квадратная)
                int size;
                file >> size;
                //записываем коэффициенты
                vector<double> kf;
                for (int i = 0; i < size * size; i++) {
                    double temp;
                    file >> temp;
                    kf.push_back(temp);
                }
                //записываем вектор b
                vector<double> b;
                for (int i = 0; i < size; i++) {
                    double temp;
                    file >> temp;
                    b.push_back(temp);
                }
                Gauss slae(size, kf, b);
                slaes.push_back(slae);
            }
        }else {
            throw IER(0);
        }
    }
    void solve(){
        for(int i = 0; i < slaes.size(); i++){
            cout << "\t\t#" << i + 1 << "\n";
            slaes[i].print();
            vector<double> x = slaes[i].solve();
            vector<double> residual = slaes[i].residual();
            for(int j = 0; j < x.size(); j++){
                cout << "\nx" << j + 1 << std::setw(16) << x[j] << std::setw(16) << "Residual: " << residual[j] << "\n";
            }
            double norm = slaes[i].norm();
            cout << "Norm: " << norm << "\n";
            double unc = slaes[i].uncertainty();
            cout << "Uncertainty: " << unc << "\n";
        }
    }
};

int main() {
    string path;
    cout << "Path to file: ";
    cin >> path;
    try {
        GaussFactory slaes(path);
        slaes.solve();
    }catch(const IER& err){
        cout << err.what();
    }
    system("pause");
    return 0;
}