// Uploads the generated site from the public/ directory to the S3 bucket from
// which it's served.
// To invoke this program, the AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY
// env vars have to be set appropriately, and the -region and -bucket flags
// have to be passed in.
package main

import (
	"context"
	"flag"
	"log"
	"os"
	"path/filepath"

	"github.com/aws/aws-sdk-go-v2/aws"
	"github.com/aws/aws-sdk-go-v2/config"
	"github.com/aws/aws-sdk-go-v2/service/s3"
)

// guessContentType guesses the HTTP content type appropriate for the given
// filename.
func guessContentType(filename string) string {
	switch filepath.Ext(filename) {
	case ".ico":
		return "image/x-icon"
	case ".png":
		return "image/png"
	case ".css":
		return "text/css"
	default:
		return "text/html"
	}
}

func main() {
	region := flag.String("region", "", "S3 region")
	bucket := flag.String("bucket", "", "S3 bucket name")
	flag.Parse()

	if len(*region) == 0 || len(*bucket) == 0 {
		log.Fatalf("region and bucket must be specified [region=%s, bucket=%s]", *region, *bucket)
	}

	cfg, err := config.LoadDefaultConfig(context.TODO(), config.WithRegion(*region))
	if err != nil {
		log.Fatal(err)
	}

	client := s3.NewFromConfig(cfg)

	// The whole contents of the public/ directory are uploaded. This code assumes
	// the directory structure is flat - there are no subdirectories.
	publicDir := "./public/"
	c, err := os.ReadDir(publicDir)
	if err != nil {
		log.Fatal(err)
	}

	for _, entry := range c {
		if !entry.IsDir() {
			file, err := os.Open(filepath.Join(publicDir, entry.Name()))
			if err != nil {
				log.Fatal(err)
			}
			defer file.Close()

			contentType := guessContentType(entry.Name())
			log.Printf("Uploading %s (%s)", entry.Name(), contentType)

			cfg := &s3.PutObjectInput{
				Bucket:      bucket,
				Key:         aws.String(entry.Name()),
				Body:        file,
				ContentType: aws.String(contentType),
			}

			_, err = client.PutObject(context.TODO(), cfg)
			if err != nil {
				log.Fatal(err)
			}
		}
	}
}
