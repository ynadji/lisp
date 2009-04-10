HOME=/Users/ynadji/Code/Lisp

cd $HOME/clojure
ant clean
git pull
ant

cd $HOME/clojure-contrib
ant clean
git pull
ant -Dclojure.jar=../clojure/clojure.jar

cd $HOME/slime
git pull

cd $HOME/slime/clojure-mode
git pull

cd $HOME/slime/swank-clojure
git pull

echo "Done!"
