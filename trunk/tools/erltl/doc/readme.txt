1. ʹ����erlyweb�е�erltl��smerl��ʵ�ֻ���Erlang��ģ������, 
   �Լ�������һ�����빤��compile_erltl.erl

   ����:
   ErlyWeb��Ŀ��ҳ:    http://erlyweb.org/
   erltl��smerl���ĵ�: http://erlyweb.org/doc/

   ��ϸ��Ϣ���Բο���ƪ�ϵ��ĵ�(���ݱȽ���):
   erltl.txt
   smerl.txt

2. ������ErlTL?
<1> <% [Exprs] %> 
    ���ʽ�ı�ǩ, �����ǩ�ڿ��԰���һ��Erlang���ʽ, ���ʽ��ֵ���Զ�̬����, 
    Լ��:
    ������ʽ���뷵��iolist, 
    Ƕ��ı��ʽ���뷵��һ��strings����binaries.

    ��չһ��С����:
    ʵ����<% func(Arg) %>�ᱻת����
    begin 
        func(Arg) 
    end
    ������һ���ԣ����ǿ�����<% [Expres] %>�а���������ʽ, ����:
    <% func1(Arg1), func2(Arg2), func3(Arg3) %>, �ᱻת����
    begin
        func1(Arg1),
        func2(Arg2),
        func3(Arg3)
    end
    ����������ʽ��ֵ����func3(Arg3)��ֵ, ֻҪ��֤func3(Arg3)���ص���һ��iolist��
    �ܱ�֤�������ʽ����iolist.
                    
<2> <%@ [FuncDecl] %>
    ����������ǩ,

    ʹ�÷�ʽ:
    <%@ func1(Arg1) %>
    <div><% Arg1 %></div>

    <%@ func2(Arg2) when Arg2 > 100 %>
    <div>Big: <% Arg2 %></div>
    <%@ func2(Arg2) when Arg2 < 10 %>
    <div>Small: <% Arg2 %></div>
    <%@ func2(Arg2) %>
    <div>Normal: <% Arg2 %></div>

<3> ErlTL�������ڱ����ļ���ʱ�򣬻��Զ���������һ��
    render(Data)�ĺ�������Ⱦ, ��������Ĺ�������Ⱦ����ҳ��.
    �����������ϸ�ṹ���Բ鿴��������ɵ�.erl�ļ�.

3. �д����������:
<1> �������ɵ�*.erl�ļ��У�����������module��export������, ���е�һ�������õġ�
    ������ⲻӰ�����Ƕ�erltl��ʹ�á�

    �������: 
    test_view.erltl��ʱ��, ���ɵ�test_view.erl�ṹ�ṹ��:
    -module(test_view).       %% Ӧ��ɾ��
    -export([]).

              %% Ӧ��ɾ��

    

-module(test_view).

    
-file("test_view", 1).
    

-compile(export_all)
   