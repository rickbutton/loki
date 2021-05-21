for i in $(find . -type f -name '*.sld' -print)
do
  scmindent < $i > scmindent.$$ && mv scmindent.$$ $i || rm scmindent.$$
done
