#include<iostream>
#include<string>
#include<array>
#include<vector>
#include<memory>
#include<algorithm>

class ExpressionSolver {
private:
    enum struct OperatorType {sum, diff, prod, div, pow};
    [[nodiscard]] static char OperatorTypeToChar(OperatorType ot) {
        switch (ot) {
            case OperatorType::sum: return '+';
            case OperatorType::diff: return '-';
            case OperatorType::prod: return '*';
            case OperatorType::div: return '/';
            case OperatorType::pow: return '^';
        }
        return '?';  // this should never happen
    }
    [[nodiscard]] static OperatorType CharToOperatorType(char c) {
        switch (c) {
            case '+': return OperatorType::sum;
            case '-': return OperatorType::diff;
            case '*': return OperatorType::prod;
            case '/': case ':': return OperatorType::div;
            case '^': return OperatorType::pow;
        }
        return OperatorType::pow;  // this should never happen
    }
    [[nodiscard]] static int OpPriority(char c) {
        switch (c) {
            case '+': case '-': return 0;
            case '*': case ':': return 1;
            case '/': return 2;
            case '^': return 3;
        }
        return -1;  // this should never happen
    }

    [[nodiscard]] static double compute_power(double base, int exp) {
        double result = 1.0;
        while (true) {
            if (exp % 2 == 1) result *= base;
            exp /= 2;
            if (exp == 0) break;
            base *= base;
        }
        return result;
    }
    struct Operator {
        OperatorType op;
        int priority_lvl;
        size_t array_idx;
        explicit Operator(char c, int nested_lvl_, size_t array_idx_) : op{CharToOperatorType(c)}, priority_lvl{nested_lvl_*4+OpPriority(c)}, array_idx{array_idx_} {}
        [[nodiscard]] double compute(double l, double r) const {
            switch (op) {
                case OperatorType::sum: return l + r;
                case OperatorType::diff: return l - r;
                case OperatorType::prod: return l * r;
                case OperatorType::div: return l / r;
                case OperatorType::pow: return r >= 0 ? compute_power(l, static_cast<int>(r)) : compute_power(1.0/l, -static_cast<int>(r));
            }
            return 0.0;  // this should never happen
        }
    };

    struct TreeNode {
        double number;
        const Operator *op;
        TreeNode *parent;
        std::array<std::unique_ptr<TreeNode>, 2> sons;
        explicit TreeNode(double number_, const Operator *op_ = nullptr, TreeNode *parent_ = nullptr) : number{number_}, op{op_}, parent{parent_}, sons{nullptr, nullptr} {}
        explicit TreeNode(const Operator *op_ = nullptr, TreeNode *parent_ = nullptr) : number{0.0}, op{op_}, parent{parent_}, sons{nullptr, nullptr} {}
        [[nodiscard]] double solve() {
            if (op != nullptr) number = op->compute(sons[0]->solve(), sons[1]->solve());
            return number;
        }
    };

    std::unique_ptr<TreeNode> _root;
    std::vector<Operator> _operators;
    std::vector<double> _numbers;

    static bool are_matching_parenthesis(char l_par, char r_par) {
        return ((l_par == '(' && r_par == ')') || (l_par == '[' && r_par == ']') || (l_par == '{' && r_par == '}'));
    }
    void BuildTree() {
        if (_operators.empty()) {
            _root = std::make_unique<TreeNode>(_numbers.front());
            return;
        }
        std::sort(_operators.begin(), _operators.end(), [](const Operator& lhs, const Operator& rhs) -> bool {
            return lhs.priority_lvl != rhs.priority_lvl ? lhs.priority_lvl < rhs.priority_lvl : lhs.array_idx > rhs.array_idx;
        });
        int side = -1;
        for (const Operator& op: _operators) {
            if(_root == nullptr) {
                _root = std::make_unique<TreeNode>(&op);
                continue;
            }
            for(TreeNode *current_node = _root.get(); current_node != nullptr; current_node = current_node->sons[side].get()) {
                side = op.array_idx < current_node->op->array_idx ? 0 : 1;
                if (current_node -> sons[side] == nullptr) {
                    current_node->sons[side] = std::make_unique<TreeNode>(&op);
                    break;
                }
            }
        }
        FillNumIntoTreeNode(_root.get());
    }
    void FillNumIntoTreeNode(TreeNode *node) {
        for (unsigned int side : {1, 0}) {
            if (node->sons[side] == nullptr) {
                node->sons[side] = std::make_unique<TreeNode>(_numbers.back());
                _numbers.pop_back();
            } else {
                FillNumIntoTreeNode(node->sons[side].get());
            }
        }
    }
public:
    explicit ExpressionSolver() : _root{nullptr} {}
    // returns false if the input is illegal
    bool parse_input(const std::string input_str) {
        char last_char_type = '?';  // can be one of the string ()0.+
        std::vector<std::string> forbidden_last_chars = {")0.", "(.+", ")", "?(", "(.+"};
        bool is_num_active = false, is_decimal = false;
        double current_num = 0.0, decimal_mult = 1.0;
        int digit = 0, sign = 1;
        std::vector<char> parenthesis;
        for (char c : input_str) {
            if (c == '(' || c == '[' || c == '{') {
                if(is_num_active || forbidden_last_chars[0].find(last_char_type) != std::string::npos) return false;
                parenthesis.push_back(c);
                last_char_type = '(';
            } else if (c == ')' || c == ']' || c == '}') {
                if (parenthesis.empty() || !are_matching_parenthesis(parenthesis.back(), c) || forbidden_last_chars[1].find(last_char_type) != std::string::npos) return false;
                parenthesis.pop_back();
                last_char_type = ')';
            } else if (c >= '0' && c <= '9') {
                if (forbidden_last_chars[2].find(last_char_type) != std::string::npos) return false;
                digit = static_cast<int>(c-'0');
                if (!is_num_active) {
                    is_num_active = true;
                    current_num = static_cast<double>(digit);
                } else if (!is_decimal) {
                    current_num = current_num * 10 + digit;
                } else {
                    decimal_mult *= 0.1;
                    current_num += digit * decimal_mult;
                }
                last_char_type = '0';
            } else if (c == '.' || c == ',') {
                if(is_decimal || !is_num_active || last_char_type != '0') return false;
                is_decimal = true;
                decimal_mult = 1.0;
                last_char_type = '.';
            } else if (c == '-' && forbidden_last_chars[3].find(last_char_type) != std::string::npos) {
                if (sign != 1) return false;
                sign = -1;
                last_char_type = '0';
            } else if (c == '+' || c == '-' || c == '*' || c == '/' || c == ':' || c == '^') {
                if(!is_num_active || forbidden_last_chars[4].find(last_char_type) != std::string::npos) return false;
                is_num_active = false;
                is_decimal = false;
                _numbers.emplace_back(current_num * sign);
                sign = 1;
                current_num = 0.0;
                _operators.emplace_back(c, parenthesis.size(), _operators.size());
                last_char_type = '+';
            }
        }
        if(!parenthesis.empty() || !is_num_active) return false;
        _numbers.push_back(current_num * sign);
        BuildTree();
        return _numbers.empty();
    }
    [[nodiscard]] double solve() { return _root->solve(); }
};

int main() {
    std::string input_str;
    std::cout << "Please write me the expression you want to solve:" << std::endl;
    std::getline(std::cin, input_str);
    ExpressionSolver es;
    if (!es.parse_input(input_str)) {
        std::cout << "The input line has some problems. Failed to read the input expression" << std::endl;
        return 1;
    }
    try {
        double result = es.solve();
        std::cout << "The result of the expression is : " << result << std::endl;
    } catch(const std::exception& e) {
        std::cerr << "The computation failed with the following error:\n" << e.what() << std::endl;
    }
    return 0;
}