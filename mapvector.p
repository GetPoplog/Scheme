


define mapvector(v,f);
    let i, n = datalength(v) in
        {% for i from 1 to n do f(subscrv(i,v)) endfor %}
    endlet
enddefine;
