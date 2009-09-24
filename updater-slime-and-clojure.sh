HOME=/home/yacin/Code/Lisp

if [ -d $HOME/clojure ]; then
	cd $HOME/clojure
	ant clean
	git pull
	ant
else
	cd $HOME
	git clone git://github.com/richhickey/clojure.git
	cd clojure
	ant
fi

if [ -d $HOME/clojure-contrib ]; then
	cd $HOME/clojure-contrib
	ant clean
	git pull
	ant -Dclojure.jar=../clojure/clojure.jar
else
	cd $HOME
	git clone git://github.com/richhickey/clojure-contrib.git
	cd clojure-contrib
	ant -Dclojure.jar=../clojure/clojure.jar
fi

if [ -d $HOME/slime ]; then
	cd $HOME/slime
	git pull
else
	cd $HOME
	git clone git://github.com/nablaone/slime.git
fi

if [ -d $HOME/slime/clojure-mode ]; then
	cd $HOME/slime/clojure-mode
	git pull
else
	cd $HOME/slime
	git clone git://github.com/jochu/clojure-mode.git
fi

if [ -d $HOME/slime/swank-clojure ]; then
	cd $HOME/slime/swank-clojure
	git pull
else
	cd $HOME/slime
	git clone git://github.com/jochu/swank-clojure.git
fi

echo "Done!"
