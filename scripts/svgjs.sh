SVGS="$@"
JS=$(cat <<EOF
<script>
"use strict";
(function () {
for (var i = 1; true; ++i) {
  var e = document.getElementById("gnuplot_plot_"+ i);
  if (e) {
    (function (eCopy) {
      eCopy.onclick = function () { togglePath(eCopy); };
    })(e);
  } else {
    break;
  }
}

function togglePath(e) {
  for (var c = e.firstChild; c; c = c.nextSibling) {
    if (c.nodeName == "path") {
      if (c.style.display == "none") {
        c.style.display = "block";
      } else {
        c.style.display = "none";
      }
    } else {
      togglePath(c);
    }
  }
}
})();
</script>
EOF
)

for svg in $SVGS
do
        if [ "$(grep '"use strict"' $svg)" == "" ]
        then
                sed --in-place -e 's/<\/svg>//g' $svg &&\
                echo "$JS" >>$svg &&\
                echo "</svg>" >>$svg
        fi
done

