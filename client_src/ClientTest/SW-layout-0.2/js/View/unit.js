function Unit( startX, startY, startZ, sex ){
	//Gender: 0 = female, 1 = male;

	this.x = startX;
	this.y = starty;
	this.z = startZ;
	this.gender = sex;

	if(gender){
		this.model = maleModel;
	} else {
		this.model = femaleModel;
	}
}