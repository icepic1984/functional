#include <functional>
#include <iostream>

template <typename A, typename B, typename C>
auto partial(A a, const std::function<C(A,B)>& f) 
{
    return std::function<C(B)>([=](B b) { return f(a, b); });
}

template <typename A, typename B, typename C>
auto curry(const std::function<C(A,B)>& f)
{
    return std::function<std::function<C(B)>(A)>([=](A a) { return partial(a, f); });
}

template <typename A, typename B, typename C>
auto uncurry(const std::function<std::function<C(B)>(A)>& f)
{
    return std::function<C(A, B)>([=](A a, B b) { return f(a)(b); });
}

template <typename A, typename B, typename C>
auto compose(const std::function<C(B)>& f, const std::function<B(A)>& g)
{
    return std::function<C(A)>([=](A a) { return f(g(a)); });
}

int main()
{
    std::function<int(int, int)> f([](int a, int b) { return a + b; });
    std::function<int(int, int)> g([](int a, int b) { return a + b; });

    std::function<int(int)> f1([](int b) { return b + 10; });
    std::function<int(int)> g1([](int a) { return a + 20; });

    std::cout << partial(10, f)(100) << std::endl;
    std::cout << uncurry(curry(g))(10,20) <<std::endl;
    std::cout << compose(f1,g1)(10) << std::endl;
}


