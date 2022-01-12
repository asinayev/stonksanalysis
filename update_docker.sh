last_commit=$(git log --format="%H" -n 1)

echo "$last_commit"

echo ":inoremap esc <Esc>" > /tmp/vimcmd.txt
echo "/checkout" >> /tmp/vimcmd.txt
echo "wdwa$last_commit esc" >> /tmp/vimcmd.txt
#echo "o esc" >> /tmp/vimcmd.txt
echo ":wq" >> /tmp/vimcmd.txt

vim -s /tmp/vimcmd.txt Dockerfile

git add Dockerfile
git commit -m 'updating dockerfile'
git push
