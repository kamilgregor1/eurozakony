/* requires D3 + https://github.com/Caged/d3-tip */
d3.scatterplotwithlineplot = function() {
  unit_circle = true;
  function scatterplotwithlineplot(selection) {
    selection.each(function(d, i) {
      //options
      var data = (typeof(data) === "function" ? data(d) : d.data),
          lines = (typeof(lines) === "function" ? lines(d) : d.lines),
          margin = (typeof(margin) === "function" ? margin(d) : d.margin),
          axes = (typeof(axes) === "function" ? axes(d) : d.axes),
          minmax = (typeof(minmax) === "function" ? minmax(d) : d.minmax),
          size = (typeof(size) === "function" ? size(d) : d.size),
          unit_circle_val = (typeof(unit_circle) === "function" ? unit_circle(d) : unit_circle);
      
      // chart sizes
      var width = size['width'] - margin.left - margin.right,
          height = size['height'] - margin.top - margin.bottom;
      
      //scales
      var xScale = d3.scale.linear()
							 .domain([minmax['x']['min'], minmax['x']['max']])
							 .range([0, width])

      var yScale = d3.scale.linear()
							 .domain([minmax['y']['min'], minmax['y']['max']])
							 .range([height, 0])

      var rScale = d3.scale.linear()
							 .domain([minmax['r']['min'],minmax['r']['max']])
							 .range([minmax['rrange']['min'],minmax['rrange']['max']]);

      //axes
      var xAxis = d3.svg.axis()
        .scale(xScale)
        .orient("bottom");
        //.ticks(5);
        //.tickSize(16, 0);  
      var yAxis = d3.svg.axis()
        .scale(yScale)
        .orient("left");
        //.ticks(5); 
      

      var element = d3.select(this);
      
		//Create X axis
      element.append("g")
			.attr("class", "axis x-axis")
			.attr("transform", "translate(0," + height + ")")
			.call(xAxis);
		
		//Create Y axis
      element.append("g")
			.attr("class", "axis y-axis")
			.call(yAxis);
      
      limits = {"x":[minmax["x"]["min"],minmax["x"]["max"]],"y":[minmax["y"]["min"],minmax["y"]["max"]]}
      for (k in lines) {
        ps = linecross(lines[k],limits);
        if (ps.length == 2) {
          lines[k].x1 = ps[0][0];
          lines[k].y1 = ps[0][1];
          lines[k].x2 = ps[1][0];
          lines[k].y2 = ps[1][1];
        }
        way = get_sign(lines[k].b,lines[k].n1,lines[k].n2);
        lines[k].path =[corners(lines[k],limits,way),corners(lines[k],limits,-1*way)];
      }
      
      //ellipse ~ unit_circle
      if (unit_circle_val) {
          element.selectAll(".ellipse")
              .data([0])
            .enter()
              .append("ellipse")
                .attr("cx", xScale(0))
                .attr("cy", yScale(0))
                .attr("rx", Math.abs(xScale(0)-xScale(0)))
                .attr("ry", Math.abs(yScale(0)-yScale(0)))
                .attr("fill-opacity",0)
                .attr("stroke","red")
                .style("stroke-dasharray", ("0"));
      }
      
      //lines
      var line = element.selectAll ('.line')
         .data(lines)
         .enter()
            .append("line")
         .attr("x1",function(d) {return xScale(d.x1)})
         .attr("y1",function(d) {return yScale(d.y1)})
         .attr("x2",function(d) {return xScale(d.x2)})
         .attr("y2",function(d) {return yScale(d.y2)})
         .attr("id", function (d, i) {return "q-" + i;})
         .attr("class", function(d) {
		   		if (typeof(d['class'] != 'undefined')) return d['class'];
		   		else return 'line';
		   })
		    //putting it here and not in css, because it is used for generating png:
		 .attr("stroke","gray")
		 .attr("stroke-width","0")
		 .attr("opacity", 0.15)
		 
         .on('mouseover', tip.show)
         .on('mouseout', tip.hide);
      
      //points
      element.selectAll(".circle")
        .data(data)
		   .enter()
		 .append("circle")
		   .attr("cx", function(d) {
		   		return xScale(d.x);
		   })
		   .attr("cy", function(d) {
		   		return yScale(d.y);
		   })
		   .attr("r", function(d) {
		   		return rScale(d.r);
		   })
		   .attr("class", function(d) {
		   		if (typeof(d['class'] != 'undefined')) return d['class'];
		   		else return 'circle';
		   })
		   .attr("fill",function(d) {return d.color})
		   .attr("stroke",function(d) {return d.color})
		   .attr("fill-opacity",function(d) {return d.opacity})
		   .on('mouseover', tip.show)
           .on('mouseout', tip.hide);
     
	
      //axis labels
	  element.append("text")
			.attr("class", "x-label label")
			.attr("text-anchor", "end")
			.attr("x", width)
			.attr("y", height-5)
			.text(axes['labels']['x']);
	  element.append("text")
			.attr("class", "y label")
			.attr("text-anchor", "end")
			.attr("y", 5)
			.attr("x", 0)
			.attr("dy", ".75em")
			.attr("transform", "rotate(-90)")
			.text(axes['labels']['y']);
			 
	    
	  // putting it here and not in css, because it is used for generating png: 
	  element.selectAll(".domain")
	        	.attr("fill","none")
				.attr("fill-opacity",0)
				.attr("stroke","black")
				.attr("stroke-width",1);
      element.selectAll(".tick")
                .attr("fill-opacity",0)
                .attr("stroke","#000")
                .attr("stroke-width",1);
      element.selectAll("text")
                .attr("font-family","sans-serif")
                .attr("font-size",11)
      element.selectAll(".label")
                .attr("font-size",15)
                .attr("font-weight","bold")
                //convert dy("0.71em") to dy("10"), inkscape feature https://www.ruby-forum.com/topic/5505193 :
      element.selectAll("text")
                .attr("dy",function(d) {
                    if (d3.select(this).attr("dy")) {
                        em = parseFloat(d3.select(this).attr("dy").replace("em",""));
                        if (d3.select(this).attr("font-size"))
                            px = parseFloat(d3.select(this).attr("font-size"));
                        else
                            px = 11;
                        return em*px + "px";
                    } else {
                        return 0;
                    }
                })
    });
  }
  scatterplotwithlineplot.data = function(value) {
    if (!arguments.length) return value;
    data = value;
    return scatterplotwithlineplot;
  };
  scatterplotwithlineplot.lines = function(value) {
    if (!arguments.length) return value;
    lines = value;
    return scatterplotwithlineplot;
  };    
  scatterplotwithlineplot.margin = function(value) {
    if (!arguments.length) return value;
    margin = value;
    return scatterplotwithlineplot;
  };
  scatterplotwithlineplot.axes = function(value) {
    if (!arguments.length) return value;
    axes = value;
    return scatterplotwithlineplot;
  };
  scatterplotwithlineplot.minmax = function(value) {
    if (!arguments.length) return value;
    minmax = value;
    return scatterplotwithlineplot;
  };
  scatterplotwithlineplot.size = function(value) {
    if (!arguments.length) return value;
    size = value;
    return scatterplotwithlineplot;
  };
  scatterplotwithlineplot.unit_circle = function(value) {
    if (!arguments.length) return value;
    unit_circle = value;
    return scatterplotwithlineplot;
  };
  return scatterplotwithlineplot;
  
  
    function corners(l,e,o) {
      //l = {"a":0,"b":1}  //line, a and slope, i.e., y=a+bx
      //e = {"x": [-10,10], "y": [-10,10]} //limits
      //o = 1 //orientation -1 or 1

      //crossing x0, x1  
      //crossing y0, y1
      outp = linecross (l,e);
      
      out = [];

      //vertices
      for (i=0;i<=1;i++){
        for (j=0;j<=1;j++){
          if (o*(l.a+l.b*e.x[i]-e.y[j]) > 0)
            outp.push([e.x[i],e.y[j]]);
        }
      }
      //sort the outps, anticlockwise
      if (outp.length > 0) {
        mid = [0,0];
        for (i in outp) {
          mid[0] += outp[i][0];
          mid[1] += outp[i][1];
        }
        mid[0] = mid[0] / outp.length;
        mid[1] = mid[1] / outp.length;
        for (i in outp) {
          p = outp[i][1] - mid[1];
          q = outp[i][0] - mid[0];
          if (q != 0)
            outp[i][2] = Math.atan(p/q) + (q<0 ? Math.PI : 0);
          else
            outp[i][2] = Math.PI/2 + Math.PI*sign(p);
        }
        outp = outp.sort(function(w,z) {
          return w[2] > z[2];
        });
        for (i in outp) {
          outp[i].splice(2,1);
          out.push({"x":outp[i][0],"y":outp[i][1]});
        }
      }
      return out;
    }
  
  function linecross (l,e) {
      out = [];
      //crossing x0, x1
      for (i=0;i<=1;i++){
        Y = l.a + l.b*e.x[i];
        if ((Y > e.y[0]) && (Y < e.y[1]))
          out.push([e.x[i],Y]);
      }
      //crossing y0, y1
      for (j=0;j<=1;j++){
        if (l.b != 0) {
          X = (e.y[j] - l.a)/l.b;
          if ((X > e.x[0]) && (X < e.x[1]))
            out.push([X,e.y[j]]);
        }
      }
      return out;
    }

    function get_sign(b,d1,d2) {
      t = b*d1-d2;
      if (t > 0) return 1;
      if (t < 0) return -1;
      return 0;
    }
  
}
