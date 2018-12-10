FROM_NS='day-10'
FROM_NS2="${FROM_NS//-/_}"
FROM_YEAR='2018'

TO_NS='day-11'
TO_NS2="${TO_NS//-/_}"
TO_YEAR='2018'

cp "$FROM_YEAR/$FROM_NS/src/$FROM_NS2/core.clj" "$TO_YEAR/$TO_NS/src/$TO_NS2"
cp "$FROM_YEAR/$FROM_NS/.gitignore" "$TO_YEAR/$TO_NS"
cp -r "$FROM_YEAR/$FROM_NS/src/linked_list" "$TO_YEAR/$TO_NS/src"

cp "$FROM_YEAR/$FROM_NS/test/$FROM_NS2/core_test.clj" "$TO_YEAR/$TO_NS/test/$TO_NS2"
