set -xe

nvc --help > output

maxcols=$(awk 'length > max { max = length } END { print max }' output)
limit=72

if [ "$maxcols" -gt $limit ]; then
   echo "FAILED: --help output is too wide!"
   awk "length >= $limit { print \$0 }" output
   exit 1
fi

