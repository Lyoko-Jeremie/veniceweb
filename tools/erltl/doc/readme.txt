1. 使用了erlyweb中的erltl和smerl来实现基于Erlang的模板语言, 
   自己增加了一个编译工具compile_erltl.erl

   链接:
   ErlyWeb项目主页:    http://erlyweb.org/
   erltl和smerl的文档: http://erlyweb.org/doc/

   详细信息可以参考两篇老的文档(内容比较早):
   erltl.txt
   smerl.txt

2. 技术点ErlTL?
<1> <% [Exprs] %> 
    表达式的标签, 这个标签内可以包含一个Erlang表达式, 表达式的值可以动态计算, 
    约束:
    这个表达式必须返回iolist, 
    嵌入的表达式必须返回一个strings或者binaries.

    扩展一个小技巧:
    实质上<% func(Arg) %>会被转换成
    begin 
        func(Arg) 
    end
    根据这一特性，我们可以在<% [Expres] %>中包含多个表达式, 例如:
    <% func1(Arg1), func2(Arg2), func3(Arg3) %>, 会被转换成
    begin
        func1(Arg1),
        func2(Arg2),
        func3(Arg3)
    end
    所以这个表达式的值就是func3(Arg3)的值, 只要保证func3(Arg3)返回的是一个iolist就
    能保证整个表达式返回iolist.
                    
<2> <%@ [FuncDecl] %>
    函数声明标签,

    使用方式:
    <%@ func1(Arg1) %>
    <div><% Arg1 %></div>

    <%@ func2(Arg2) when Arg2 > 100 %>
    <div>Big: <% Arg2 %></div>
    <%@ func2(Arg2) when Arg2 < 10 %>
    <div>Small: <% Arg2 %></div>
    <%@ func2(Arg2) %>
    <div>Normal: <% Arg2 %></div>

<3> ErlTL编译器在编译文件的时候，会自动在最顶部添加一个
    render(Data)的函数来渲染, 这个函数的功能是渲染整个页面.
    这个函数的详细结构可以查看编译后生成的.erl文件.

3. 有待解决的问题:
<1> 编译生成的*.erl文件中，包含了两次module和export的声明, 其中第一次是无用的。
    这个问题不影响我们对erltl的使用。

    例如编译: 
    test_view.erltl的时候, 生成的test_view.erl结构结构是:
    -module(test_view).       %% 应该删除
    -export([]).

              %% 应该删除

    

-module(test_view).

    
-file("test_view", 1).
    

-compile(export_all)
   