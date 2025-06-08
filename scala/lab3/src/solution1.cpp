#include<iostream>
#include<type_traits>
using namespace std;

template <typename T, typename enabled = void >
class Triangle{};

template <typename T>
class Triangle<T, typename std::enable_if_t<!std::is_integral_v<T>>>{
    private:
        /*Point*/ std::int32_t _a{}, _b{}, _c{};
        T _a_data, _b_data, _c_data;
    public:
        Triangle() = default;
        inline double square() const noexcept{ return 1.0; }
        inline double perimeter() const noexcept { return _a + _b + _c; }
};

template <typename T>
class Triangle<T, typename std::enable_if_t<std::is_integral_v<T>>>{
    private:
        /*Point*/ std::int32_t _a{}, _b{}, _c{};
        T _a_data, _b_data, _c_data;
    public:
        Triangle() = default;
        inline double square() const noexcept{ return 1.0; }
        inline double perimeter() const noexcept { return _a + _b + _c; }
        inline double mass_center() const noexcept { return 0.0; }
};

int main(int argc, char** argv){
    Triangle<int> a;
    Triangle<double> b;
    printf("%f\n%f\n", a.mass_center(), b.mass_center());
    return 0;
}
