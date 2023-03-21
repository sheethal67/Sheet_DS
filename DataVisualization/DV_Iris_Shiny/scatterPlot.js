"use strict";
scatterPlot(svg, data, width, height, options);
	
function scatterPlot(svg, Data, WIDTH, HEIGHT, options){
	let xName = options.names[0];
	let yName = options.names[1];
	let xAxisLabel = options.axislabels[0];
	let yAxisLabel = options.axislabels[1];
	
	let margin = {top: 20, right: 20, bottom: 100, left: 100};
	let width = WIDTH - margin.left - margin.right + 40;    // plot width of Chart container
	let height = HEIGHT - margin.top - margin.bottom + 40; // plot height of Chart container

	svg.selectAll("*").remove();

	svg.append("rect")
		.attr("width", WIDTH)
		.attr("height", HEIGHT)
		.style("stroke", "lightgray")
		.style("fill", "none");

  	let chartContainer = svg.append("g")
    		//.attr("transform", "translate(" + 60 + "," + 20 + ")");

    		.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

	chartContainer.append("rect")
		.attr("width", width)
		.attr("height", height)
		.style("stroke", "lightgray")
		.style("fill", "none");

	let xMin = d3.min(Data, function(d){return d[xName]});
	let xMax = d3.max(Data, function(d){return d[xName]});
	let yMin = d3.min(Data, function(d){return d[yName]});
	let yMax = d3.max(Data, function(d){return d[yName]});
	let xScale = d3.scaleLinear().domain([xMin, xMax]).range([0, width]);
	let yScale = d3.scaleLinear().domain([yMin,yMax]).range([height,0]); // See the range!!

	let colorScale = d3.scaleOrdinal()
	    .domain(["setosa","versicolor","virginica"])
	    .range(d3.schemeCategory10)

	chartContainer.append('g')
  	.selectAll("dot")
		.data(Data)
		.enter()
		.append("circle")
    		.attr("cx", function (d) { return xScale(d[xName]); } )
    		.attr("cy", function (d) { return yScale(d[yName]); } )
    		.attr("r", 7)
    		.style("fill",function(d){return colorScale(d.Species);})

		.on("mouseover", function(d,i){
			d3.select(this)
				.style("stroke", "black");
			 tooltip.transition()
               .style("opacity", 0.9);
        tooltip.html("<b>"  +d.Species+ "<br/> "
                            +"Sepal_Length: "+d.Sepal_Length+ "<br/> "
                            +"Sepal_Width: "+ d.Sepal_Width + "<br/>"
                            +"Petal_Length: "+d.Petal_Length+ "<br/> "
                            +"Petal_Width: "+d.Petal_Width)
               .style("left", (d3.event.pageX + 5) + "px") 
               .style("top", (d3.event.pageY - 28) + "px");
		})
		
		.on("mouseout", function(d,i){
			d3.select(this)
				.style("stroke", "none");	
			toolTipText.text("");
		});
	
	var tooltip = d3.select("body").append("div")
    .append("rect")
    .style("background-color", "steelblue")
    .style("padding", "5px")
    .style("font-size","10px")
    .attr("class", "tooltip")
    .style("opacity",0);
    
  // Add axes
	let xAxis = d3.axisBottom().scale(xScale);
	chartContainer.append('g').attr('transform', 'translate(0,' + height + ')').call(xAxis);
	let yAxis = d3.axisLeft(yScale);
	chartContainer.append('g').call(yAxis);

  	// Add Labels
 	chartContainer.append("text")             
    		.attr("text-anchor", "middle")  // Centre the text 
		.attr("transform", "translate("+ (width/2) +","+(height+(margin.bottom/3))+")")  // centre below axis
		.text(xAxisLabel);

  	chartContainer.append("text")
    		.attr("text-anchor", "middle")  // Centre the text
		.attr("transform", "translate("+ (-margin.left/3) +","+(height/2)+")rotate(-90)")  // text is drawn off the screen top left, move down and out and rotate
		.text(yAxisLabel);
}
