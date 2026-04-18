for i in {1..100};do((i%3))&&x=||x=Fizz;((i%5))||x+=Buzz;echo ${x:-$i};done
