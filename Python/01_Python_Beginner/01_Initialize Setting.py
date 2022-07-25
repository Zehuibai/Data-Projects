#################################################
### Install Pip and package
#################################################

# https://blog.csdn.net/weixin_39622521/article/details/110465503
# https://pypi.org/project/pip/
# cd C:\pip
# python setup.py install

# pip install pandas
# pip3 install pandas
# python -m pip install pandas
# sudo pip install pandas

# Check python version
# py -3 --version


 
#################################################
### Comment code 
#################################################

# in Jupyter notebook ipynb document
# Ctrl + ' to comment on multiple lines


#################################################
### Python Variables
#################################################

"""
Python 变量命名规则：

变量名必须以字母或下划线字符开头
变量名称不能以数字开头
变量名只能包含字母数字字符和下划线（A-z、0-9 和 _）
变量名称区分大小写（age、Age 和 AGE 是三个不同的变量）
"""

## Python 允许您在一行中为多个变量赋值
x, y, z = "Orange", "Banana", "Cherry"
print(x,z)

## Python 的 print 语句通常用于输出变量。如需结合文本和变量，Python 使用 + 字符
z = y = x = "awesome"
print("Python is " + x + " " + y)

## 在函数内部创建变量时，该变量是局部变量，只能在该函数内部使用。
## 要在函数内部创建全局变量，您可以使用 global 关键字。
def myfunc():
  global x
  x = "fantastic"
myfunc()
print("Python is " + x)
 
#################################################
### Python 数据类型
#################################################
def typefunc(x):
  return 'Type of x is, {}'.format(type(x))
typefunc(x = "Hello World")
typefunc(x = 29)
typefunc(x = 29.5)
typefunc(x = 1j)
typefunc(x = ["apple", "banana", "cherry"])
typefunc(x = ("apple", "banana", "cherry"))
typefunc(x = range(6))
typefunc(x = {"name" : "Bill", "age" : 63})
typefunc(x = {"apple", "banana", "cherry"})
typefunc(x = frozenset({"apple", "banana", "cherry"}))
typefunc(x = b"Hello")

### 可以使用 int()、float() 和 complex() 方法从一种类型转换为另一种类型
### !!! 无法将复数转换为其他数字类型
27e4       ## 浮点数也可以是带有“e”的科学数字，表示 10 的幂
int(15E2)  ## Int 或整数是完整的数字，正数或负数，没有小数，长度不限
2+3j       ## 复数用 "j" 作为虚部编写




#################################################
### Python Directory and Files Management
#################################################
import os
## Get Current Directory
os.getcwd()
print(os.getcwd())

## Changing Directory
os.chdir('C:\\')

## List Directories and Files
os.listdir()

## Renaming a Directory or a File
os.rename('old.txt','new_one')

## Removing Directory or File
os.remove('old.txt')


