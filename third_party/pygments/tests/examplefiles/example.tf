variable "key_name" {
    description = "Name of the SSH keypair to use in AWS."
}

variable "key_path" {
    description = "Path to the private portion of the SSH key specified."
}

variable "aws_region" {
    description = "AWS region to launch servers."
    default = "us-west-2"
    somevar = true
}

# Ubuntu Precise 12.04 LTS (x64)
variable "aws_amis" {
    default = {
        eu-west-1 = "ami-b1cf19c6"
        us-east-1 = "ami-de7ab6b6"
        us-west-1 = "ami-3f75767a"
        us-west-2 = "ami-21f78e11"
    }
}






provider "aws" {
    access_key = "${myvar}" 
    secret_key = "your aws secret key"
    region = "us-east-1"
}
/* multiline

  comment

*/


# Single line comment
resource "aws_instance" "example" {
    ami = "ami-408c7f28"
    instance_type = "t1.micro"
    key_name      = "your-aws-key-name"
}

# Create our Heroku application. Heroku will
# automatically assign a name.
resource "heroku_app" "web" {}

# Create our DNSimple record to point to the
# heroku application.
resource "dnsimple_record" "web" {
  domain = "${var.dnsimple_domain}"


  # heroku_hostname is a computed attribute on the heroku
  # application we can use to determine the hostname
  value = "${heroku_app.web.heroku_hostname}"

  type = "CNAME"
  ttl = 3600
}

# The Heroku domain, which will be created and added
# to the heroku application after we have assigned the domain
# in DNSimple
resource "heroku_domain" "foobar" {
    app = "${heroku_app.web.name}"
    hostname = "${dnsimple_record.web.hostname}" 
}


# Specify the provider and access details
provider "aws" {
    region = "${var.aws_region}" 
    value = ${file("path.txt")} 
}

# Our default security group to access
# the instances over SSH and HTTP
resource "aws_security_group" "default" {
    name = "terraform_example"
    description = "Used in the terraform"

    # SSH access from anywhere
    ingress {
        from_port = 22
        to_port = 22
        protocol = "tcp"
        cidr_blocks = ["0.0.0.0/0"]
    }

    # HTTP access from anywhere
    ingress {
        from_port = 80
        to_port = 80
        protocol = "tcp"
        cidr_blocks = ["0.0.0.0/0"]
    }
}


resource "aws_elb" "web" {
  name = "terraform-example-elb"

  # The same availability zone as our instance
  availability_zones = ["${aws_instance.web.availability_zone}"]

  listener {
    instance_port = 80
    instance_protocol = "http"
    lb_port = 80
    lb_protocol = "http"
  }

  # The instance is registered automatically
  instances = ["${aws_instance.web.id}"]
}


resource "aws_instance" "web" {
  # The connection block tells our provisioner how to
  # communicate with the resource (instance)
  connection {
    # The default username for our AMI
    user = "ubuntu"

    # The path to your keyfile
    key_file = "${var.key_path}"
  }

  instance_type = "m1.small"

  # Lookup the correct AMI based on the region
  # we specified
  ami = "${lookup(var.aws_amis, var.aws_region)}"

  # The name of our SSH keypair you've created and downloaded
  # from the AWS console.
  #
  # https://console.aws.amazon.com/ec2/v2/home?region=us-west-2#KeyPairs:
  #
  key_name = "${var.key_name}"

  # Our Security group to allow HTTP and SSH access
  security_groups = ["${aws_security_group.default.name}"]

  # We run a remote provisioner on the instance after creating it.
  # In this case, we just install nginx and start it. By default,
  # this should be on port 80
  provisioner "remote-exec" {
    inline = [
        "sudo apt-get -y update",
        "sudo apt-get -y install nginx",
        "sudo service nginx start"
    ]
  }
}

