arr1 = [['SG0016949819','Gojek'],
['SG0016927486','GrabFood'],
['SG0016952324','DBS Bank'],
['SG0016934029','Comfort Delgro'],
['SG0016946747','DBS Bank'],
['SG0016928374','Gojek'],
['SG0016937516','foodpanda'],
['SG0016928593','GrabFood'],
['SG0016934563','Gojek']]

arr2 = [['Grab'],['Comfort DelGro'],['FoodPanda'],['DBS']]

var regex = new RegExp( arr2.join( "|" ), "i")

var arr3 = arr1.filter(x => {return x[1].match(regex)})

console.log(arr3)


