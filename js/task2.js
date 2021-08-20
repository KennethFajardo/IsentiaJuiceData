arr1 = [['SG0016949819','Gojek'],
['SG0016927486','GrabFood'],
['SG0016952324','DBS Bank'],
['SG0016934029','Comfort Delgro'],
['SG0016946747','DBS Bank'],
['SG0016928374','Gojek'],
['SG0016937516','foodpanda'],
['SG0016928593','GrabFood'],
['SG0016934563','Gojek']]

arr2 = [['GrabFood', 'Food Delivery'],['Comfort DelGro', 'Transport'],['FoodPanda', 'Food Delivery'],['DBS', 'Banking & Finance']]

var merged = [];
for(i=0; i<arr2.length; i++){
	var regex = new RegExp(arr2[i][0], "i")
	var temp = arr1.filter(x => { return x[1].match(regex) })
    temp.forEach(x => { merged.push(x.concat(arr2[i][1]))})
}

console.log(merged)